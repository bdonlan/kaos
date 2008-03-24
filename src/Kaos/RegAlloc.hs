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

module Kaos.RegAlloc (regAlloc) where

import Kaos.CAOS
import Kaos.VirtRegister
import Kaos.KaosM

-- XXX STUB
regAlloc :: CAOS VirtRegister -> KaosM (CAOS CAOSRegister)
regAlloc = return . map regLine

regLine :: CAOSLine VirtRegister -> CAOSLine CAOSRegister
regLine (CAOSLine l) = CAOSLine $ map regTok l

regTok :: CAOSToken VirtRegister -> CAOSToken CAOSRegister
regTok (CAOSRegister (VR i)) 
	| i < 100
	= CAOSRegister (CAOSReg i)
	| otherwise
	= error "Out of CAOS registers, bug bd_ to implement the vreg allocation properly"
regTok (CAOSLiteral l) = CAOSLiteral l
regTok (CAOSConst c) = CAOSConst c
