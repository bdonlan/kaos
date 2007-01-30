module Kaos.Core (Core(..), CoreBlock(..), CoreLine(..), CoreToken(..),
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
import Kaos.PrettyM
import Data.Char
import Control.Arrow

type Note = ()

data CoreToken =
    TokenLiteral String
  | TokenSlot    (GenAccess Slot)
  | TokenConst   ConstValue
  deriving (Data, Typeable)

shortAccess NoAccess = ""
shortAccess ReadAccess = "r"
shortAccess WriteAccess = "w"
shortAccess MutateAccess = "rw"

instance Show CoreToken where
    show (TokenLiteral l) = map toUpper $ show l
    show (TokenSlot (SA s ac)) = "$" ++ (show s) ++ "(" ++ (shortAccess ac) ++ ")"
    show (TokenConst c) = "#" ++ show c

data CoreLine t =
    CoreLine [CoreToken]
  | CoreAssign Slot Slot -- dest src
  | CoreConst  Slot ConstValue
  | CoreNote   Note
  | CoreTouch  (GenAccess Slot)
  | CoreCond   [CoreToken] (CoreBlock t) (CoreBlock t)
  | CoreLoop   (CoreBlock t)
  | CoreTypeSwitch { ctsSlot :: Slot
                   , ctsNum  :: CoreLine t 
                   , ctsStr  :: CoreLine t
                   , ctsObj  :: CoreLine t
                   }
  -- TODO: CoreCondition, CoreLoop etc
  deriving (Show, Data, Typeable)

instance Functor CoreLine where
    fmap f (CoreLine l) = CoreLine l
    fmap f (CoreAssign s1 s2) = CoreAssign s1 s2
    fmap f (CoreConst s cv) = CoreConst s cv
    fmap f (CoreNote n) = CoreNote n
    fmap f (CoreTouch s) = CoreTouch s
    fmap f (CoreCond cond if_ else_) =
        CoreCond cond (fmap f if_) (fmap f else_)
    fmap f (CoreTypeSwitch slot n s o) =
        CoreTypeSwitch slot (fmap f n) (fmap f s) (fmap f o)
    fmap f (CoreLoop body ) =
        CoreLoop (fmap f body) 

showLine :: CoreLine f -> PrettyM ()
showLine (CoreLine l) =
    emitLine $ "NORMAL " ++ unwords (map show l)
showLine (CoreAssign dest src) =
    emitLine $ "ASSIGN " ++ unwords (map show [dest, src])
showLine (CoreConst t cv) =
    emitLine $ "CONST= " ++ (show t) ++ " " ++ (show cv)
showLine (CoreNote n) =
    emitLine $ "NOTE " ++ show n
showLine (CoreTouch t) =
    emitLine $ "TOUCH " ++ show t
showLine (CoreCond cond ifb elsb) = prefixFirst "IF " $ do
    prefixFirst "DOIF " $ emitLine $ unwords (map show cond)
    withIndent 5 $ showBlock ifb
    emitLine "ELSE"
    withIndent 5 $ showBlock elsb
    emitLine "ENDI"
showLine (CoreTypeSwitch slot num str obj) = prefixFirst "CTS " $ do
    emitLine $ "CONTROL: " ++ show slot
    prefixFirst "NUM: " $ showLine num
    prefixFirst "STR: " $ showLine str
    prefixFirst "OBJ: " $ showLine obj
showLine (CoreLoop body ) =
    prefixFirst "LOOP " $ showBlock body
showLine x = prefixFirst "XXX UNCODED SHOWLINE " $ emitLine (show $ fmap (const ()) x)

showBlock :: CoreBlock f -> PrettyM ()
showBlock (CB l) = mapM_ (showLine . fst) l

lineNormalize (CoreTypeSwitch s cn cs co)
    | slotType s == typeNum
    = lineNormalize cn
    | slotType s == typeStr
    = lineNormalize cs
    | slotType s == typeObj
    = lineNormalize co
lineNormalize l = l

coreNormalize :: Core t -> Core t
coreNormalize (CB l) = CB $ map (first lineNormalize) l

newtype CoreBlock t = CB [(CoreLine t, t)]
    deriving (Show, Eq, Ord, Read, Data, Typeable)
type Core t = CoreBlock t

instance Functor CoreBlock where
    fmap f (CB l) = CB $ map (\(cl, t) -> (fmap f cl, f t)) l
    
dumpCore :: Show t => Core t -> String
dumpCore = runPretty . showBlock

data GenAccess t = SA t AccessType
    deriving (Show, Ord, Eq, Data, Typeable)

data AccessType = NoAccess | ReadAccess | WriteAccess | MutateAccess
    deriving (Show, Ord, Eq, Data, Typeable)

mergeAccess x y | x == y = x
mergeAccess NoAccess x = x
mergeAccess MutateAccess _ = MutateAccess
mergeAccess ReadAccess WriteAccess = MutateAccess
mergeAccess x y = mergeAccess y x

