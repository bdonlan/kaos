module Kaos.Toplevel (
	KaosUnit(..),
	KaosSource,
    Macro(..),
    MacroArg(..),
    MacroContext,
    FreeMacro,
    Data.Word.Word8,
    Data.Word.Word16,
) where

import Data.Word (Word8, Word16)

import Kaos.AST

data MacroArg = MacroArg    { maName    :: String
                            , maType    :: CAOSType
                            , maOutput  :: Bool
                            , maDefault :: Maybe ConstValue
                            }
                            deriving (Show, Eq, Ord)

data Macro = Macro  { mbName    :: String
                    , mbArgs    :: [MacroArg]
                    , mbCode    :: Statement String
                    , mbRetType :: CAOSType
                    , mbContext :: MacroContext
                    }

instance Show Macro where
    show (Macro n a c t _) = "Macro { mbName = " ++ (show n) ++ ", mbArgs = " ++ (show a) ++ ", mbCode = " ++ (show c) ++ ", mbRetType = " ++ (show t) ++ " }"

type MacroContext = String -> Maybe Macro

type FreeMacro = MacroContext -> Macro
instance Show FreeMacro where
    show f = show $ f undefined

data KaosUnit = InstallScript (Statement String)
              | MacroBlock  FreeMacro
              | RemoveScript (Statement String)
              | AgentScript { asFamily  :: !Word8
                            , asGenus   :: !Word8
                            , asSpecies :: !Word16
                            , asScript  :: !Word16
                            , asCode    :: Statement String
                            }
              deriving (Show)

type KaosSource = [KaosUnit]
