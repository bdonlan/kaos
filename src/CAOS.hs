module CAOS (CAOS, CAOSBlock, CAOSLine(..), CAOSToken(..),
             CAOSRegister(..)) where

import Data.Ix
import AST

data CAOSToken r =
    CAOSLiteral String
  | CAOSRegister r
  | CAOSConst ConstValue
  deriving (Show)

data CAOSLine r =
    CAOSLine [CAOSToken r]
    deriving (Show)

type CAOSBlock r = [CAOSLine r]
type CAOS r = CAOSBlock r

newtype CAOSRegister = CAOSReg Int
    deriving (Show, Read, Eq, Ord, Ix)

instance Bounded CAOSRegister where
    minBound = CAOSReg 0
    maxBound = CAOSReg 99
