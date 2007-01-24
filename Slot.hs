module Slot (Slot(..), SlotAccess(..), AccessType(..),
             mergeAccess, SlotAllocT, runSlotAllocT, newSlot) where

import SeqT

newtype Slot = Slot Int
    deriving (Show, Read, Eq, Ord, Enum)

data SlotAccess = SA { saSlot :: Slot, saAccess :: AccessType }
    deriving (Show, Read, Eq)

data AccessType = NoAccess | ReadAccess | WriteAccess | MutateAccess
    deriving (Show, Read, Eq)

mergeAccess x y | x == y = x
mergeAccess NoAccess x = x
mergeAccess MutateAccess _ = MutateAccess
mergeAccess ReadAccess WriteAccess = MutateAccess
mergeAccess x y = mergeAccess y x

newtype SlotAllocT m a = SAT (SeqT Slot m a)
    deriving (Monad, MonadTrans)

newSlot :: Monad m => SlotAllocT m Slot
newSlot = SAT getNext

runSlotAllocT :: Monad m -> SlotAllocT m a -> m a
runSlotAllocT (SAT m) = runSeqT (Slot 0) m
