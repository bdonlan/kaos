module KaosM (runKaosM, KaosM, newSlot, debugKM) where

import SeqT
import Slot
import Control.Monad.Identity
import Control.Monad.Trans

newtype KaosM a = KM (SeqT Slot IO a)
    deriving (Monad, Functor)

debugKM s = KM (lift $ putStrLn s)

newSlot = KM getNext

runKaosM (KM m) = runSeqT (Slot 0) m

