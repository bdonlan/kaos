module SeqT (SeqT, getNext, runSeqT) where

import Control.Monad.State
import Control.Monad.Trans

newtype SeqT e m a = S (StateT e m a)
    deriving (Monad, MonadTrans, Functor)

getNext :: (Enum e, Monad m) => SeqT e m e
getNext = S $ do
    n <- get
    put $ succ n
    return n

runSeqT :: (Enum e, Monad m) => e -> SeqT e m r -> m r
runSeqT i (S m) = evalStateT m i
