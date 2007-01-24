module Core (Core, CoreBlock, CoreLine(..), lineAccess, CoreToken(..),
             Note(..)) where

import Slot
import AST
import qualified Data.Map as M

type Note = ()

data CoreToken =
    TokenLiteral String
  | TokenSlot    SlotAccess
  | TokenConst   ConstValue
  deriving (Show)

lineAccess (CoreLine t)
    = M.toList $ foldl addAccess M.empty (concatMap findAccess t)
    where
        findAccess (TokenSlot s) = [s]
        findAccess _ = []
        addAccess m (SA s ac) = M.alter updateKey s m
            where
            updateKey Nothing = Just ac
            updateKey (Just a') = Just $ a' `mergeAccess` ac
lineAccess (CoreAssign s1 s2) =
    [(s1, WriteAccess), (s2, ReadAccess)]
lineAccess (CoreConst s1 _) = [(s1, WriteAccess)]
lineAccess _ = []

data CoreLine =
    CoreLine [CoreToken]
  | CoreAssign Slot Slot -- dest src
  | CoreConst  Slot ConstValue
  | CoreNote   Note
  -- TODO: CoreCondition, CoreLoop etc
  deriving (Show)

type CoreBlock = [CoreLine]
type Core = CoreBlock
