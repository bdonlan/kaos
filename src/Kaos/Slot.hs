module Kaos.Slot (Slot(..)) where

import Kaos.AST
import Data.Ord

data Slot = Slot { slotId :: Int
                 , slotTrace :: Maybe () -- TODO
                 , slotType :: CAOSType
                 }

instance Show Slot where
    show (Slot id _ t) = (show id) ++ ":" ++ (show t)

equating f a b  =  f a == f b

instance Enum Slot where
    fromEnum = slotId
    toEnum i = Slot i Nothing typeAny

instance Eq Slot where
    (==) = equating slotId

instance Ord Slot where
    compare = comparing slotId
