module Slot (Slot(..)) where

import AST
import Data.Ord

data Slot = Slot { slotId :: Int
                 , slotTrace :: Maybe () -- TODO
                 , slotType :: CAOSType
                 }
    deriving (Show)

equating f a b  =  f a == f b

instance Enum Slot where
    fromEnum = slotId
    toEnum i = Slot i Nothing typeAny

instance Eq Slot where
    (==) = equating slotId

instance Ord Slot where
    compare = comparing slotId
