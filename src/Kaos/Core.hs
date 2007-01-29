module Kaos.Core (Core, CoreBlock, CoreLine(..), lineAccess, CoreToken(..),
             Note(..),
             AccessType(..),
             GenAccess(..),
             coreNormalize,
             dumpCore,
             mergeAccess,
             ) where

import Kaos.Slot
import Kaos.AST
import Data.Generics
import qualified Data.Map as M

type Note = ()

data CoreToken t =
    TokenLiteral String
  | TokenSlot    (GenAccess t)
  | TokenConst   ConstValue
  deriving (Data, Typeable)

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
  | CoreTypeSwitch { ctsSlot :: t
                   , ctsNum  :: CoreLine t
                   , ctsStr  :: CoreLine t
                   , ctsObj  :: CoreLine t
                   }
  -- TODO: CoreCondition, CoreLoop etc
  deriving (Show, Data, Typeable)

instance Functor CoreLine where
    fmap f (CoreLine l) = CoreLine $ map (fmap f) l
    fmap f (CoreAssign dest src) = CoreAssign (f dest) (f src)
    fmap f (CoreConst dest cv) = CoreConst (f dest) cv
    fmap f (CoreTouch (SA s a)) = CoreTouch (SA (f s) a)
    fmap f (CoreTypeSwitch s cn cs co) = CoreTypeSwitch (f s) (fmap f cn) (fmap f cs) (fmap f co)
    fmap _ (CoreNote n) = CoreNote n

lineNormalize (CoreTypeSwitch s cn cs co)
    | slotType s == typeNum
    = lineNormalize cn
    | slotType s == typeStr
    = lineNormalize cs
    | slotType s == typeObj
    = lineNormalize co
lineNormalize l = l

coreNormalize :: Core Slot -> Core Slot
coreNormalize = map lineNormalize

type CoreBlock t = [CoreLine t]
type Core t = CoreBlock t

dumpCore :: Show t => Core t -> String
dumpCore = unlines . map ("* "++) . map show

data GenAccess t = SA t AccessType
    deriving (Show, Ord, Eq, Data, Typeable)

data AccessType = NoAccess | ReadAccess | WriteAccess | MutateAccess
    deriving (Show, Ord, Eq, Data, Typeable)

mergeAccess x y | x == y = x
mergeAccess NoAccess x = x
mergeAccess MutateAccess _ = MutateAccess
mergeAccess ReadAccess WriteAccess = MutateAccess
mergeAccess x y = mergeAccess y x

