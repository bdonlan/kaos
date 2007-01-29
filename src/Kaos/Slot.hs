module Kaos.Slot (Slot(..)) where

import Kaos.AST
import Data.Ord
import Data.Generics

data Slot = Slot { slotId :: Int
                 , slotTrace :: Maybe () -- TODO
                 , slotType :: CAOSType
                 } deriving (Data, Typeable)

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
