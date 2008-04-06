{-
   Kaos - A compiler for creatures scripts
   Copyright (C) 2005-2008  Bryan Donlan

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module Kaos.Core (Core, CoreBlock(..), CoreLine(..), CoreToken(..),
             Note(..),
             Kaos.AST.AccessType(..),
             AccessMap(..),
             GenAccess(..), SlotAccess,
             dumpCore, dumpCoreLine,
             mergeAccess,
             LineAccess(..),
             Folder
             ) where

import Kaos.Slot
import Kaos.AST
import Data.Generics
import qualified Data.Map as M
import Kaos.PrettyM
import Data.Char
import Data.Monoid

data Note = PrivateNote String
          | ContextNote KaosContext
        deriving (Show, Eq, Ord, Typeable, Data)

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

type Folder = (Slot -> Maybe ConstValue) -> Maybe [CoreLine ()]
instance Show Folder where show _ = "(...)"

data CoreLine t =
    CoreLine [CoreToken]
  | CoreAssign Slot Slot -- dest src
  | CoreInlineAssign
        { ciaLevel       :: Int
        , ciaTargUser    :: Bool
        , ciaDestSlot    :: Slot
        , ciaReplacement :: [CoreToken]
        }
  | CoreInlineFlush Int
  | CoreConst  Slot ConstValue
  | CoreNote   Note
  | CoreTouch  (GenAccess Slot)
  | CoreCond   [CoreToken] (CoreBlock t) (CoreBlock t)
  | CoreLoop   (CoreBlock t)
  | CoreFoldable Folder (CoreLine t)
-- first slot is a temporary. It will be overwritten by Kaos.Targ
-- this is horrible, fix it later :|
--
-- strict to keep me from making it undefined and later wondering why debug
-- shows cause crashes
  | CoreTargReader !Slot Slot (CoreBlock t)
  | CoreTargWriter Slot (CoreBlock t)
  | CoreTargZap
  -- TODO: CoreCondition, CoreLoop etc
  deriving (Show, Data, Typeable)

instance Functor CoreLine where
    fmap _ CoreTargZap = CoreTargZap
    fmap _ (CoreLine l) = CoreLine l
    fmap _ (CoreAssign s1 s2) = CoreAssign s1 s2
    fmap _ (CoreConst s cv) = CoreConst s cv
    fmap _ (CoreNote n) = CoreNote n
    fmap _ (CoreTouch s) = CoreTouch s
    fmap _ (CoreInlineFlush l) = CoreInlineFlush l
    fmap _ (CoreInlineAssign level targuser ds repl) =
        CoreInlineAssign level targuser ds repl
    fmap f (CoreTargReader ts s b) = CoreTargReader ts s (fmap f b)
    fmap f (CoreTargWriter s b) = CoreTargWriter s (fmap f b)
    fmap f (CoreCond cond if_ else_) =
        CoreCond cond (fmap f if_) (fmap f else_)
    fmap f (CoreLoop body ) =
        CoreLoop (fmap f body) 
    fmap f (CoreFoldable folder body) =
        CoreFoldable folder (fmap f body)

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
showLine (CoreLoop body ) =
    prefixFirst "LOOP " $ showBlock body
showLine (CoreTargReader ts slot body) = prefixFirst ("TARG ") $ do
    emitLine $ "< temp:" ++ (show ts) ++ ", " ++ (show slot)
    showBlock body
showLine (CoreTargWriter slot body) = prefixFirst ("TARG ") $ do
    emitLine $ "> " ++ (show slot)
    showBlock body
showLine (CoreFoldable _ body) = prefixFirst ("FOLD ") $ showLine body
showLine (CoreInlineFlush level) = emitLine $ "FLUSH " ++ (show level) ++ "+"
showLine (CoreInlineAssign level targuser ds repl) =
    emitLine $ "INLINESET @" ++ (show level) ++ targstr ++ " " ++ (show ds) ++ " = " ++ 
                (unwords $ map show repl)
    where
        targstr
            | targuser
            = "(targ)"
            | otherwise
            = ""
showLine CoreTargZap = emitLine "TARGZAP"
--showLine x = prefixFirst "XXX UNCODED SHOWLINE " $ emitLine (show $ fmap (const ()) x)

showBlock :: Show f => CoreBlock f -> PrettyM ()
showBlock (CB l) = mapM_ showpair l
    where
        showpair (l', info) = prefixFirst ((show info) ++ " ") (showLine l')

newtype CoreBlock t = CB { unCB :: [(CoreLine t, t)] }
    deriving (Show, Data, Typeable)
type Core t = CoreBlock t

instance Functor CoreBlock where
    fmap f (CB l) = CB $ map (\(cl, t) -> (fmap f cl, f t)) l
    
dumpCore :: Show t => Core t -> String
dumpCore = runPretty . showBlock

dumpCoreLine :: Show t => CoreLine t -> String
dumpCoreLine = runPretty . showLine

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
instance LineAccess t => LineAccess (a, t) where getLineAccess = getLineAccess . snd

