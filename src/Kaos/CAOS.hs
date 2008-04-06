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
module Kaos.CAOS (CAOS, CAOSBlock, CAOSLine(..), CAOSToken(..),
             CAOSRegister(..)) where

import Data.Ix
import Kaos.AST
import Data.Generics

data CAOSToken r =
    CAOSLiteral String
  | CAOSRegister r
  | CAOSConst ConstValue
  deriving (Show, Data, Typeable)

data CAOSLine r =
    CAOSLine [CAOSToken r]
  | CAOSLoop (CAOS r)
    deriving (Show, Data, Typeable)

type CAOSBlock r = [CAOSLine r]
type CAOS r = CAOSBlock r

newtype CAOSRegister = CAOSReg Int
    deriving (Show, Read, Eq, Ord, Ix, Data, Typeable)

instance Bounded CAOSRegister where
    minBound = CAOSReg 0
    maxBound = CAOSReg 99
