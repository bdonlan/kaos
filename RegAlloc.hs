module RegAlloc (
    finalTranslate
) where

import Slot
import SlotAlloc
import Register
import Control.Monad.ST

finalTranslate b = do
    caosStruct <- translateBlocks b
    return $ unlines (map unwords caosStruct)

translateBlocks :: [RBlock s] -> ST s [[String]]
translateBlocks b = do
    rb <- sequence $ map translateRBlock b
    return $ concat rb

translateRBlock (RLine toks) = do
    let line = map translateRToken toks
    return [line]
translateRBlock (RBlock blocks) = translateBlocks blocks

translateRToken (RTLiteral s) = s
translateRToken (RTRegister (R n))
    | n >= 100
    = error $ "XXX: register max is 100 for now, sorry"
    | n < 10
    = "va0" ++ show n
    | otherwise
    = "va" ++ show n
