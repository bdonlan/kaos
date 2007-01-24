module VirtRegister (VirtRegister(..), VRegAllocT, runVRegAllocT, newVReg) where

import SeqT

newtype VirtRegister = VR Int
    deriving (Show, Read, Eq, Ord, Enum)

newtype RegAllocT m a = RT (SeqT VirtRegister m a)
    deriving (Monad, MonadTrans)

runVRegAllocT :: (Monad m) => VRegAllocT m a -> m a
runVRegAllocT (RT m ) = runSeqT (VR 0) m

newVReg :: (Monad m) => VRegAllocT m Register
newVReg = RT getNext
