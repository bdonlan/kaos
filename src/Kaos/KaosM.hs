module Kaos.KaosM (runKaosM, KaosM, newSlot, debugKM, MonadKaos(..),
                  isSet, whenSet, debugDump, KaosDiag, KaosContext(..),
                  internalError, compileError, warning, KaosDiagM(..)
                  ) where

import Kaos.SeqT
import Kaos.Slot
import Kaos.AST
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Set as S

class Monad m => MonadKaos m where
    liftK :: KaosM a -> m a

instance (MonadKaos m) => MonadKaos (StateT v m) where
    liftK = lift . liftK

instance (MonadKaos m) => MonadKaos (ReaderT a m) where
    liftK = lift . liftK

instance (Monoid w, MonadKaos m) => MonadKaos (WriterT w m) where
    liftK = lift . liftK

instance (MonadKaos m) => MonadKaos (SeqT i m) where
    liftK = lift . liftK

instance MonadKaos KaosM where
    liftK = id

data KaosContext = KaosContext  { kcFileName :: String
                                , kcLineNum  :: Int
                                }

instance Show KaosContext where
    show = showContext . Just

showContext :: Maybe KaosContext -> String
showContext Nothing = "(unknown)"
showContext (Just kc) = (kcFileName kc) ++ (show $ kcLineNum kc)

data KaosDiag = KaosDiag    { kdFatal   :: Bool
                            , kdInternal:: Bool
                            , kdContext :: Maybe KaosContext
                            , kdMessage :: String
                            } deriving (Show)

iceBase :: KaosDiag
iceBase = KaosDiag { kdFatal = True, kdInternal = True, kdContext = Nothing, kdMessage = undefined}

data KState = KState
    { kFlags    :: S.Set String
    , kContext  :: Maybe KaosContext
    , kFailed   :: Bool
    }

instance Error KaosDiag where
    noMsg    = strMsg "unknown error"
    strMsg s = iceBase { kdMessage = "Internal compiler error: " ++ s }

initState :: S.Set String -> KState
initState flags = KState flags Nothing False

type KaosM' a = ErrorT KaosDiag (StateT KState (SeqT Slot IO)) a
newtype KaosM a = KM { unKM :: KaosM' a }
    deriving (Functor, Monad)

instance MonadError KaosDiag KaosM where
    throwError e = KM $ do
        s <- get
        throwError $ e { kdContext = kContext s }
    catchError (KM m) f = KM $ catchError m $ \e -> unKM (f e)

class MonadKaos m => KaosDiagM m where
    checkpoint    :: a -> m a -> m a
    context       :: KaosContext -> m a -> m a

internalError :: MonadKaos m => String -> m ()
internalError = liftK . fail

compileError :: MonadKaos m => String -> m ()
compileError s = liftK $
    throwError $ KaosDiag { kdFatal = True, kdInternal = False, kdMessage = s, kdContext = Nothing }

warning :: MonadKaos m => String -> m ()
warning = liftK . kmWarning

instance KaosDiagM b => KaosDiagM (StateT s b) where
    checkpoint d m = StateT $ \st -> checkpoint (d, st) $ runStateT m st
    context ctx m  = StateT $ \st -> context ctx $ runStateT m st

instance KaosDiagM b => KaosDiagM (SeqT s b) where
    checkpoint d m = SeqT   $ checkpoint d (unSeqT m)
    context ctx  m = SeqT   $ context ctx  (unSeqT m)

instance KaosDiagM KaosM where
    checkpoint    = kmCheckpoint
    context       = kmContext

printDiag :: MonadIO m => KaosDiag -> m ()
printDiag d = liftIO $ do
    putStr $ showContext (kdContext d)
    putStr ": "
    if (kdFatal d)
        then putStr "Error: "
        else putStr "Warning: "
    putStrLn (kdMessage d)

kmWarning :: String -> KaosM ()
kmWarning message = KM $ do
    ctx <- fmap kContext get
    printDiag (KaosDiag False False ctx message)

saveCtx :: KaosM' a -> KaosM' a
saveCtx m = do
    ctx <- fmap kContext get
    ret <- m
    modify $ \s -> s { kContext = ctx }
    return ret

kmCheckpoint :: a -> KaosM a -> KaosM a
kmCheckpoint dummyVal (KM m) = KM $ saveCtx $ catchError m report
    where
        report err = do
            s <- get
            put $ s { kFailed = True }
            when (not (kFailed s) || not (kdInternal err)) $ printDiag err
            return dummyVal

kmContext :: KaosContext -> KaosM r -> KaosM r
kmContext ctx (KM m) = KM $ saveCtx $ do
    modify $ \s -> s { kContext = Just ctx }
    m

newSlot :: MonadKaos m => CAOSType -> m Slot
newSlot t = liftK $ do
    s <- KM (lift . lift $ getNext)
    return $ s { slotType = t }
debugKM :: MonadKaos m => String -> m ()
debugKM s = liftK $ KM (liftIO $ putStrLn s)

isSet :: MonadKaos m => String -> m Bool
isSet s = liftK $ KM (gets $ S.member s . kFlags)

whenSet :: MonadKaos m => String -> m () -> m ()
whenSet s m = do
    f <- isSet s
    when f m

debugDump :: MonadKaos m => String -> String -> m ()
debugDump s d = whenSet s (debugKM d)

runKaosM' :: [String] -> KaosM v -> IO (Either KaosDiag (Maybe v))
runKaosM' flags m     = runSeqT (toEnum 0 :: Slot)
                      $ flip evalStateT (initState $ S.fromList flags)
                      $ runErrorT 
                      $ unKM (checkpoint Nothing (fmap Just m))


runKaosM :: [String] -> KaosM v -> IO (Maybe v)
runKaosM flags m = do
    res <- runKaosM' flags m
    case res of
        Left diag -> do
            putStrLn "WARNING: runKaosM' returned Left, this should not happen. Diagnostic triggering:"
            printDiag diag
            return Nothing
        Right ret -> return ret
