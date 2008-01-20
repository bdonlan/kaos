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

import qualified Data.Map as M
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
                    , mbContext :: MacroContext
                    }
                    deriving (Show)
type MacroContext = M.Map String Macro

type FreeMacro = MacroContext -> Macro
instance Show FreeMacro where
    show f = show $ f M.empty

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
