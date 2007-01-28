module KaosM (runKaosM, KaosM, newSlot, debugKM) where

import SeqT
import Slot
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

class Monad m => MonadKaos m where
    liftK :: KaosM a -> m a

instance (MonadKaos m) => MonadKaos (StateT v m) where
    liftK = lift . liftK

instance (MonadKaos m) => MonadKaos (ReaderT a m) where
    liftK = lift . liftK

instance (Monoid w, MonadKaos m) => MonadKaos (WriterT w m) where
    liftK = lift . liftK

instance MonadKaos KaosM where
    liftK = id

newtype KaosM a = KM (SeqT Slot IO a)
    deriving (Monad, Functor)

newSlot :: MonadKaos m => m Slot
newSlot = liftK $ KM getNext
debugKM :: MonadKaos m => String -> m ()
debugKM s = liftK $ KM (lift $ putStrLn s)

runKaosM (KM m) = runSeqT (toEnum 0) m

