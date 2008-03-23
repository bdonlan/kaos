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
) where

import Data.Word (Word8, Word16)

import Kaos.AST
import Kaos.Slot
import qualified Data.Map as M

data MacroArg = MacroArg    { maName    :: String
                            , maType    :: CAOSType
                            , maDefault :: Maybe ConstValue
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
            }

instance Show Macro where
    show (Macro n typ a c t l _) = "Macro { mbName = " ++ (show n) ++ ", mbType = " ++ (show typ) ++ ", mbArgs = " ++ (show a) ++ ", mbCode = " ++ (show c) ++ ", mbRetType = " ++ (show t) ++ ", mbLexVars = " ++ (show l) ++ " }"

type MacroContext = String -> Maybe Macro

data KaosUnit = InstallScript (Statement String)
              | MacroBlock  Macro
              | RemoveScript (Statement String)
              | OVDecl      { ovName    :: String
                            , ovIndex   :: Maybe Int
                            , ovType    :: CAOSType
                            }
              | AgentScript { asFamily  :: !Word8
                            , asGenus   :: !Word8
                            , asSpecies :: !Word16
                            , asScript  :: !Word16
                            , asCode    :: Statement String
                            }
              deriving (Show)

type KaosSource = [KaosUnit]
