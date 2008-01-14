module Kaos.Slot (Slot(..)) where

import Kaos.AST
import Data.Ord
import Data.Generics

data Slot = Slot { slotId :: Int
                 , slotTrace :: Maybe () -- TODO
                 , slotType :: CAOSType
                 } deriving (Data, Typeable)

instance Show Slot where
    show (Slot i _ t) = (show i) ++ ":" ++ (show t)

equating :: Eq b => (a -> b) -> a -> a -> Bool
equating f a b  =  f a == f b

instance Enum Slot where
    fromEnum = slotId
    toEnum i = Slot i Nothing typeAny

instance Eq Slot where
    (==) = equating slotId

instance Ord Slot where
    compare = comparing slotId
