module Core (Core(..)) where

import Slot
import AST
import qualified Data.Map as M

type Note = ()

data CoreToken =
    TokenLiteral String
  | TokenSlot    SlotAccess
  | TokenConst   ConstValue
  deriving (Show)

lineAccess l = error "lineAccess TODO"

data CoreLine =
    CoreLine [CoreToken]
  | CoreAssign Slot Slot -- dest src
  | CoreConst  Slot ConstValue
  | CoreNote   Note
  -- TODO: CoreCondition, CoreLoop etc
  deriving (Show)

type CoreBlock = [CoreLine]
type Core = CoreBlock
