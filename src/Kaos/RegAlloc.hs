module Kaos.RegAlloc (regAlloc) where

import Kaos.CAOS
import Kaos.VirtRegister
import Kaos.KaosM

-- XXX STUB
regAlloc :: CAOS VirtRegister -> KaosM (CAOS CAOSRegister)
regAlloc = return . map regLine

regLine (CAOSLine l) = CAOSLine $ map regTok l

regTok (CAOSRegister (VR i)) = CAOSRegister (CAOSReg i)
regTok (CAOSLiteral l) = CAOSLiteral l
regTok (CAOSConst c) = CAOSConst c
