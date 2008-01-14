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
