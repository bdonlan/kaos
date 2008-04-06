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
module Kaos.Emit (emitCaos, emitConst) where

import Kaos.CAOS
import Kaos.AST
import Data.Char

--- XXX unflattenable
emitCaos :: [CAOSLine CAOSRegister] -> String
emitCaos c = unlines $ map emitLine c

emitLine :: CAOSLine CAOSRegister -> String
emitLine (CAOSLine l) = unwords $ map emitToken l
emitLine (CAOSLoop l) = unlines $ map emitLine l

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
