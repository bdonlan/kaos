module Kaos.Emit (emitCaos, emitConst) where

import Kaos.CAOS
import Kaos.AST
import Data.Char

--- XXX unflattenable
emitCaos :: [CAOSLine CAOSRegister] -> String
emitCaos c = unlines $ map emitLine c

emitLine :: CAOSLine CAOSRegister -> String
emitLine (CAOSLine l) = unwords $ map emitToken l

emitToken :: CAOSToken CAOSRegister -> String
emitToken (CAOSLiteral s) = map toUpper s
emitToken r@(CAOSRegister (CAOSReg i)) =
    case show i of
        s@[_] -> "VA0" ++ s
        s@[_,_] -> "VA" ++ s
        _ -> error $ "ICE: Register out of range (bug bd_): " ++ show r
emitToken (CAOSConst cv) = emitConst cv

emitConst :: ConstValue -> String
emitConst (CInteger i) = show i
emitConst (CString s) = s
emitConst (CFloat f) = show f
