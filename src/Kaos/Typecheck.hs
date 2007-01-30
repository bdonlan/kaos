module Kaos.Typecheck (
    typecheck, TypeCheckT, sameType, typeIs
    ) where

import Kaos.KaosM
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Kaos.Core
import Kaos.AST
import Kaos.Slot
import Data.Maybe
import Data.Generics

import qualified Data.Map as M

newtype TypeCheckT m a = TCT (TCT m a)
    deriving (Monad, MonadKaos, Functor)

class (MonadKaos m', Monad m) => MonadTC m m' | m -> m' where
    liftTC :: TypeCheckT m' a -> m a

instance MonadKaos m => MonadTC (TypeCheckT m) m where
    liftTC = id

instance (MonadKaos m', Monoid w, MonadTC m m') => MonadTC (WriterT w m) m' where
    liftTC = lift . liftTC

sameType s1 s2 = liftTC . TCT $ bindSlots s1 s2 >> return s1
typeIs s1 t    = liftTC . TCT $ constrainSlotType t s1

data St = St { typeVars :: M.Map Int (Either Int CAOSType)
             , slotVars :: M.Map Slot Int
             } deriving (Show)

initSt = St M.empty M.empty

type TCT m a = StateT St m a

getSlotVar' slot = do
    s <- get
    let slotVar = fromMaybe (slotId slot) (M.lookup slot $ slotVars s)
    case fromMaybe (Right typeAny) (M.lookup slotVar $ typeVars s) of
        Left indirect -> do
            beenThere <- asks (slotVar `elem`)
            when (beenThere) $ do
                ctx <- ask
                st  <- get
                fail $ "Loop detected: " ++ show (ctx, st)
            put $ s { slotVars = M.insert slot indirect (slotVars s) }
            local (slotVar:) $ getSlotVar' slot
        Right _ -> return slotVar

getSlotVar :: (Monad m, MonadState St m) => Slot -> m Int
getSlotVar = flip runReaderT [] . getSlotVar'

getSlotType slot = do
    varId <- getSlotVar slot
    Right t <- liftM (fromMaybe (Right typeAny)) (gets (M.lookup varId . typeVars))
    return t

constrainSlotType t slot = do
    oldType <- getSlotType slot
    varId <- getSlotVar slot
    let newType = t `typeAnd` oldType
    when (newType == typeVoid) $ fail "Unsatisfiable type constraint"
    modify $ \s -> s { typeVars = M.insert varId (Right newType) $ typeVars s }

bindSlots s1 s2 = do
    t1 <- getSlotType s1
    t2 <- getSlotType s2
    when (t2 /= typeAny) $ constrainSlotType t2 s1
    when (t1 /= typeAny) $ constrainSlotType t1 s2
    var1 <- getSlotVar s1
    var2 <- getSlotVar s2
    
    when (var1 /= var2) $ do
    modify $ \s -> s { typeVars = M.insert var1 (Left var2) $ typeVars s}

translateSlot state slot = evalState m state
    where
        m = do
            t <- getSlotType slot
            return $ slot { slotType = t }

typecheck :: (MonadKaos m) => (TypeCheckT m (Core ())) -> m (Core ())
typecheck (TCT m) = flip evalStateT initSt $ do
    core <- m
    s <- get
    return . coreNormalize $ everywhere (mkT $ translateSlot s) core

