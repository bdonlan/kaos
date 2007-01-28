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

instance Show t => Show (CoreToken t) where
    show (TokenLiteral l) = "l:" ++ show l
    show (TokenSlot s) = "s:" ++ show s
    show (TokenConst c) = "c:" ++ show c

instance Functor CoreToken where
    fmap f (TokenSlot (SA t a)) = TokenSlot (SA (f t) a)
    fmap _ (TokenLiteral s)     = TokenLiteral s
    fmap _ (TokenConst c)       = TokenConst c


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

instance Functor CoreLine where
    fmap f (CoreLine l) = CoreLine $ map (fmap f) l
    fmap f (CoreAssign dest src) = CoreAssign (f dest) (f src)
    fmap f (CoreConst dest cv) = CoreConst (f dest) cv
    fmap f (CoreTouch (SA s a)) = CoreTouch (SA (f s) a)
    fmap _ (CoreNote n) = CoreNote n

type CoreBlock t = [CoreLine t]
type Core t = CoreBlock t

dumpCore :: Show t => Core t -> String
dumpCore = unlines . map ("* "++) . map show

data GenAccess t = SA t AccessType
    deriving (Show, Ord, Eq)

data AccessType = NoAccess | ReadAccess | WriteAccess | MutateAccess
    deriving (Show, Ord, Eq)

mergeAccess x y | x == y = x
mergeAccess NoAccess x = x
mergeAccess MutateAccess _ = MutateAccess
mergeAccess ReadAccess WriteAccess = MutateAccess
mergeAccess x y = mergeAccess y x

