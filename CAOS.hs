module CAOS (CAOS(..), CAOSRegister(..)) where

data CAOSToken r =
    CAOSLiteral String
  | CAOSRegister r
  | CAOSConst ConstValue
  deriving (Show)

data CAOSLine r =
    CAOSLine [CAOSToken r]
    deriving (Show)

type CAOSBlock = [CAOSLine]
type CAOS = CAOSBlock

newtype CAOSRegister = CAOSReg Int
    deriving (Show, Read, Eq, Ord, Ix)

instance Bounded CAOSRegister where
    minBound = CAOSReg 0
    maxBound = CAOSReg 99
