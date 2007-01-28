module Emit (emitCaos) where

import CAOS
import AST
import Data.Char

--- XXX unflattenable
emitCaos c = unlines $ map emitLine c

emitLine (CAOSLine l) = unwords $ map emitToken l

emitToken (CAOSLiteral s) = map toUpper s
emitToken r@(CAOSRegister (CAOSReg i)) =
    case show i of
        s@[_] -> "VA0" ++ s
        s@[_,_] -> "VA" ++ s
        s -> error $ "ICE: Register out of range: " ++ show r
emitToken (CAOSConst cv) = emitConst cv

emitConst (CInteger i) = show i
emitConst (CString s) = s
emitConst (CFloat f) = show f
