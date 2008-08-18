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

module Kaos.Toplevel (
	KaosUnit(..),
	KaosSource,
    Macro(..),
    MacroType(..),
    MacroArg(..),
    defaultMacro,
    MacroContext,
    Data.Word.Word8,
    Data.Word.Word16,
    addMacroToCtx,
    macroPrototype
) where

import Data.Word (Word8, Word16)
import Data.List
import Data.Maybe
import Control.Monad

import Kaos.AST
import Kaos.Slot
import Kaos.KaosM
import qualified Data.Map as M

data MacroArg = MacroArg    { maName    :: String
                            , maType    :: CAOSType
                            }
                            deriving (Show, Eq, Ord)

data MacroType  = MacroRValue
                | MacroLValue
                | MacroIterator { miArgTypes :: [CAOSType] }
                deriving (Show, Eq, Ord)

data Macro = Macro  { mbName    :: String
                    , mbType    :: MacroType
                    , mbArgs    :: [MacroArg]
                    , mbCode    :: Statement String
                    , mbRetType :: CAOSType
                    , mbLexVars :: M.Map String Slot
                    , mbContext :: MacroContext
                    , mbRedefine:: Bool
                    }

defaultMacro :: Macro
defaultMacro =
    Macro   { mbName    = undefined
            , mbType    = undefined
            , mbArgs    = undefined
            , mbCode    = undefined
            , mbRetType = undefined
            , mbLexVars = M.empty
            , mbContext = undefined
            , mbRedefine= False
            }

instance Show Macro where
    show (Macro n typ a c t l _ r) = "Macro { mbName = " ++ (show n) ++ ", mbType = " ++ (show typ) ++ ", mbArgs = " ++ (show a) ++ ", mbCode = " ++ (show c) ++ ", mbRetType = " ++ (show t) ++ ", mbLexVars = " ++ (show l) ++ ", mbRedefine = " ++ (show r) ++ " }"

macroPrototype :: Macro -> String
macroPrototype Macro{mbName = name, mbType = macroType, mbRetType = typ, mbArgs = args} =
    "macro" ++ typPrefix ++ " " ++ name ++ "(" ++ argDesc ++ ")" ++ retndesc
    where
        typPrefix =
            case macroType of
                MacroRValue -> ""
                MacroLValue -> " set"
                MacroIterator iargs ->
                    let iargdesc = concat $ intersperse ", " (map typedesc iargs)
                    in " iterator(" ++ iargdesc ++ ")"
        argDesc = concat $ intersperse ", " (map descOneArg args)
        retndesc
            | typ == typeVoid
            = ""
            | otherwise
            = "returning " ++ typedesc typ
        typedesc t
            | t == typeNum
            = "numeric"
            | t == typeStr
            = "string"
            | t == typeObj
            = "agent"
            | t == typeVoid
            = "void"
            | t == typeAny
            = "ANY"
            | otherwise
            = "INVALID:" ++ (show typ)
        descOneArg (MacroArg argName argType)
            = (typedesc argType) ++ " " ++ argName


type MacroContext = M.Map String (M.Map Int Macro)

addMacroToCtx :: KaosDiagM m => Bool -> Macro -> MacroContext -> m MacroContext
addMacroToCtx force m@Macro{mbRedefine = redef, mbName = name, mbArgs = args} ctx
    | name == "print" || name == "anim"
    = compileError $ "Can't redefine " ++ name
    | isJust replacing && not (redef || force)
    = compileError $
        "Won't redefine pre-existing macro:\n\t" ++ (macroPrototype $ fromJust replacing)
    | not force && redef && isNothing replacing
    = do
        warning $ "Redefining a macro which previously didn't exist"
        addMacroToCtx True m ctx
    | otherwise
    = return $ M.insert name newNameGrp ctx
    where
        oldNameGrp = fromJust (M.lookup name ctx `mplus` Just M.empty)
        replacing = M.lookup (length args) oldNameGrp
        newNameGrp = M.insert (length args) m oldNameGrp

data KaosUnit = InstallScript (Statement String)
              | MacroBlock KaosContext Macro
              | RemoveScript (Statement String)
              | OVDecl      { ovName    :: String
                            , ovIndex   :: Maybe Int
                            , ovType    :: CAOSType
                            , ovErrCtx  :: KaosContext
                            }
              | AgentScript { asHead    :: Statement String
                            , asCode    :: Statement String
                            }
              deriving (Show)

type KaosSource = [KaosUnit]
