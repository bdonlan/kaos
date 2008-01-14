module Kaos.Core (Core, CoreBlock(..), CoreLine(..), CoreToken(..),
             Note,
             Kaos.AST.AccessType(..),
             AccessMap(..),
             GenAccess(..), SlotAccess,
             coreNormalize,
             dumpCore,
             mergeAccess,
             LineAccess(..), lineAccess
             ) where

import Kaos.Slot
import Kaos.AST
import Data.Generics
import qualified Data.Map as M
import Kaos.PrettyM
import Data.Char
import Control.Arrow
import Data.Monoid

type Note = ()

data CoreToken =
    TokenLiteral String
  | TokenSlot    (GenAccess Slot)
  | TokenConst   ConstValue
  deriving (Data, Typeable)

-- XXX: needs to go in AST
shortAccess :: AccessType -> String
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
-- first slot is a temporary. It will be overwritten by Kaos.Targ
-- this is horrible, fix it later :|
--
-- strict to keep me from making it undefined and later wondering why debug
-- shows cause crashes
  | CoreTargReader !Slot Slot (CoreBlock t)
  | CoreTargWriter Slot (CoreBlock t)
  | CoreTypeSwitch { ctsSlot :: Slot
                   , ctsNum  :: CoreLine t 
                   , ctsStr  :: CoreLine t
                   , ctsObj  :: CoreLine t
                   }
  -- TODO: CoreCondition, CoreLoop etc
  deriving (Show, Data, Typeable)

instance Functor CoreLine where
    fmap _ (CoreLine l) = CoreLine l
    fmap _ (CoreAssign s1 s2) = CoreAssign s1 s2
    fmap _ (CoreConst s cv) = CoreConst s cv
    fmap _ (CoreNote n) = CoreNote n
    fmap _ (CoreTouch s) = CoreTouch s
    fmap f (CoreTargReader ts s b) = CoreTargReader ts s (fmap f b)
    fmap f (CoreTargWriter s b) = CoreTargWriter s (fmap f b)
    fmap f (CoreCond cond if_ else_) =
        CoreCond cond (fmap f if_) (fmap f else_)
    fmap f (CoreTypeSwitch slot n s o) =
        CoreTypeSwitch slot (fmap f n) (fmap f s) (fmap f o)
    fmap f (CoreLoop body ) =
        CoreLoop (fmap f body) 

showLine :: Show f => CoreLine f -> PrettyM ()
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
showLine (CoreTargReader ts slot body) = prefixFirst ("TARG ") $ do
    emitLine $ "< temp:" ++ (show ts) ++ ", " ++ (show slot)
    showBlock body
showLine (CoreTargWriter slot body) = prefixFirst ("TARG ") $ do
    emitLine $ "> " ++ (show slot)
    showBlock body
--showLine x = prefixFirst "XXX UNCODED SHOWLINE " $ emitLine (show $ fmap (const ()) x)

showBlock :: Show f => CoreBlock f -> PrettyM ()
showBlock (CB l) = mapM_ showpair l
    where
        showpair (l', info) = prefixFirst ((show info) ++ " ") (showLine l')

lineNormalize :: CoreLine x -> CoreLine x
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

newtype CoreBlock t = CB { unCB :: [(CoreLine t, t)] }
    deriving (Show, Eq, Ord, Data, Typeable)
type Core t = CoreBlock t

instance Functor CoreBlock where
    fmap f (CB l) = CB $ map (\(cl, t) -> (fmap f cl, f t)) l
    
dumpCore :: Show t => Core t -> String
dumpCore = runPretty . showBlock

data GenAccess t = SA t AccessType
    deriving (Show, Ord, Eq, Data, Typeable)

type SlotAccess = GenAccess Slot

newtype AccessMap = AM { getAM :: M.Map Slot AccessType }
    deriving (Eq, Ord, Show, Typeable, Data)

instance Monoid AccessMap where
    mempty = AM M.empty
    mappend (AM a) (AM b) =
        AM $ M.filter (/= NoAccess) $ M.unionWith comb a b
        where
            comb x y | x == y = x
            comb MutateAccess _ = MutateAccess
            comb WriteAccess ReadAccess = MutateAccess
            comb NoAccess x = x
            comb x y = comb y x

-- XXX: needs to go in AST
mergeAccess :: AccessType -> AccessType -> AccessType
mergeAccess x y | x == y = x
mergeAccess NoAccess x = x
mergeAccess MutateAccess _ = MutateAccess
mergeAccess ReadAccess WriteAccess = MutateAccess
mergeAccess x y = mergeAccess y x

class LineAccess t where
    getLineAccess :: t -> AccessMap

instance LineAccess AccessMap where getLineAccess = id

lineAccess :: LineAccess t => (CoreLine t, t) -> AccessMap
lineAccess  = getLineAccess . snd
    
