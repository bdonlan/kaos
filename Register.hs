module Register (
    Register(..)
) where

import Data.STRef
import Control.Monad.ST
import LiftST

newtype Register s = R Int
    deriving (Show, Eq, Ord)

