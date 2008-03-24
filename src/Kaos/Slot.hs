{-
   Kaos - A compiler for creatures scripts
   Copyright (C) 2005-2008  Bryan Donlan

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Kaos.Slot (Slot(..), dummySlot) where

import Kaos.AST
import Data.Ord
import Data.Generics

data Slot = Slot { slotId :: Int
                 , slotName :: Maybe String
                 , slotType :: CAOSType
                 } deriving (Data, Typeable)

dummySlot :: Slot
dummySlot = Slot (-1) Nothing typeVoid

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
