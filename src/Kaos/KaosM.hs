module Kaos.KaosM (runKaosM, KaosM, newSlot, debugKM, MonadKaos(..),
                  isSet, whenSet, debugDump
                  ) where

import Kaos.SeqT
import Kaos.Slot
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
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

data KState = KState
    { kFlags :: S.Set String }

newtype KaosM a = KM (StateT KState (SeqT Slot IO) a)
    deriving (Monad, Functor)

newSlot :: MonadKaos m => m Slot
newSlot = liftK $ KM (lift getNext)
debugKM :: MonadKaos m => String -> m ()
debugKM s = liftK $ KM (lift $ lift $ putStrLn s)

isSet :: MonadKaos m => String -> m Bool
isSet s = liftK $ KM (gets $ S.member s . kFlags)

whenSet :: MonadKaos m => String -> m () -> m ()
whenSet s m = do
    f <- isSet s
    when f m

debugDump :: MonadKaos m => String -> String -> m ()
debugDump s d = whenSet s (debugKM d)

runKaosM :: [String] -> KaosM v -> IO v
runKaosM flags (KM m) = runSeqT (toEnum 0)
                      $ evalStateT m (KState $ S.fromList flags)

