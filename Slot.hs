module Slot (Slot(..), SlotAccess(..), AccessType(..),
             mergeAccess) where

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

