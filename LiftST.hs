module LiftST (MonadST(..)) where
import Control.Monad
import Control.Monad.ST
import Control.Monad.Error
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State


class Monad m => MonadST s m | m->s where
    liftST :: ST s a -> m a
instance MonadST s (ST s) where
    liftST = id
instance MonadST s m => MonadST s (ReaderT r m) where
    liftST = lift . liftST
instance (Monoid w, MonadST s m) => MonadST s (WriterT w m) where
    liftST = lift . liftST
instance (Error e, MonadST s m) => MonadST s (ErrorT e m) where
    liftST = lift . liftST
instance MonadST s m => MonadST s (ContT r m) where
    liftST = lift . liftST
instance MonadST s m => MonadST s (StateT v m) where
    liftST = lift . liftST
