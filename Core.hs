module Core (Core, CoreBlock, CoreLine(..), lineAccess, CoreToken(..),
             Note(..),
             AccessType(..),
             GenAccess(..),
             ) where

import Slot
import AST
import qualified Data.Map as M

type Note = ()

data CoreToken t =
    TokenLiteral String
  | TokenSlot    (GenAccess t)
  | TokenConst   ConstValue
  deriving (Show)


lineAccess :: (Ord t, Eq t) => CoreLine t -> [(t, AccessType)]
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

data CoreLine t =
    CoreLine [CoreToken t]
  | CoreAssign t t -- dest src
  | CoreConst  t ConstValue
  | CoreNote   Note
  | CoreTouch  (GenAccess t)
  -- TODO: CoreCondition, CoreLoop etc
  deriving (Show)

type CoreBlock t = [CoreLine t]
type Core t = CoreBlock t

data GenAccess t = SA t AccessType
    deriving (Show, Ord, Eq)

data AccessType = NoAccess | ReadAccess | WriteAccess | MutateAccess
    deriving (Show, Ord, Eq)

mergeAccess x y | x == y = x
mergeAccess NoAccess x = x
mergeAccess MutateAccess _ = MutateAccess
mergeAccess ReadAccess WriteAccess = MutateAccess
mergeAccess x y = mergeAccess y x

