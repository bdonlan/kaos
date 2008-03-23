module Kaos.SeqT (SeqT(..), getNext, runSeqT) where

import Control.Monad.State
import Control.Monad.Trans

newtype SeqT e m a = SeqT { unSeqT :: StateT e m a }
    deriving (Monad, MonadTrans, MonadIO, Functor)

getNext :: (Enum e, Monad m) => SeqT e m e
getNext = SeqT $ do
    n <- get
    put $ succ n
    return n

runSeqT :: (Enum e, Monad m) => e -> SeqT e m r -> m r
runSeqT i (SeqT m) = evalStateT m i
