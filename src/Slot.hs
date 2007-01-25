module Slot (Slot(..)) where

newtype Slot = Slot Int
    deriving (Show, Read, Eq, Ord, Enum)

