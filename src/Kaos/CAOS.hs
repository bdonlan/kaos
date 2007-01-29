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
    deriving (Show, Data, Typeable)

type CAOSBlock r = [CAOSLine r]
type CAOS r = CAOSBlock r

newtype CAOSRegister = CAOSReg Int
    deriving (Show, Read, Eq, Ord, Ix, Data, Typeable)

instance Bounded CAOSRegister where
    minBound = CAOSReg 0
    maxBound = CAOSReg 99
