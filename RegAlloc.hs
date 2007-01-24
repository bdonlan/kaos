module RegAlloc (regAlloc) where

import CAOS
import VirtRegister
import KaosM

-- XXX STUB
regAlloc :: CAOS VirtRegister -> KaosM (CAOS CAOSRegister)
regAlloc = return . map regLine

regLine (CAOSLine l) = CAOSLine $ map regTok l

regTok (CAOSRegister (VR i)) = CAOSRegister (CAOSReg i)
regTok (CAOSLiteral l) = CAOSLiteral l
regTok (CAOSConst c) = CAOSConst c
