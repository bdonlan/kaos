module ASTToCore (astToCore) where

import Core
import AST
import Slot
import KaosM

astToCore :: Statement Slot -> KaosM CoreBlock
astToCore (SExpr e) = do
    (_, out) <- runWriter $ expToCore e
    return out
astToCore (SBlock st) = fmap concat $ mapM astToCore st

expToCore :: Expression Slot -> WriterT KaosM CoreLine Slot
expToCore (ELexical s) = return s
expToCore (EBinaryOp "addv" e1 e2) = do
    s1   <- expToCore e1
    s2   <- expToCore e2
    dest <- newVReg
    tell $ CoreAssign dest s1
    tell $ CoreLine [ TokenLiteral "addv"
                    , TokenSlot (SA dest MutateAccess)
                    , TokenSlot (SA s2   ReadAccess)
                    ]
    -- XXX type info
    return dest
expToCore (EAssign e1 e2) = do
    -- TODO: determine if e1 is mutable
    s1 <- expToCore e1
    s2 <- expToCore e2
    tell $ CoreAssign s1 s2
expToCore (ECall "print" [e]) = do
    s <- expToCore e
    tell $ CoreLine [ TokenLiteral "outv"
                    , TokenSlot (SA s ReadAccess)
                    ]
expToCore e = error $ "ICE: can't expToCore: " ++ show e

