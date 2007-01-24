module ASTToCore (astToCore) where

import Core
import AST
import Slot
import KaosM
import Control.Monad.Writer

astToCore :: Statement Slot -> KaosM CoreBlock
astToCore (SExpr e) = do
    (_, out) <- runWriterT $ expToCore e
    return out
astToCore (SBlock st) = fmap concat $ mapM astToCore st

emit x = tell [x]

expToCore :: Expression Slot -> WriterT CoreBlock KaosM Slot
expToCore (EConst c) = do
    s <- lift $ newSlot
    emit $ CoreConst s c
    return s
expToCore (ELexical s) = return s
expToCore (EBinaryOp op e1 e2) = do
    s1   <- expToCore e1
    s2   <- expToCore e2
    dest <- lift $ newSlot
    emit $ CoreAssign dest s1
    emit $ CoreLine [ TokenLiteral op
                    , TokenSlot (SA dest MutateAccess)
                    , TokenSlot (SA s2   ReadAccess)
                    ]
    -- XXX type info
    return dest
expToCore (EAssign e1 e2) = do
    -- TODO: determine if e1 is mutable
    s1 <- expToCore e1
    s2 <- expToCore e2
    emit $ CoreAssign s1 s2
    return s1
expToCore (ECall "print" [e]) = do
    s <- expToCore e
    emit $ CoreLine [ TokenLiteral "outv"
                    , TokenSlot (SA s ReadAccess)
                    ]
    return $ error "XXX: void return"
expToCore (ECall "sqrt" [e]) = do
    s <- expToCore e
    result <- lift $ newSlot
    emit $ CoreLine [ TokenLiteral "setv"
                    , TokenSlot (SA result WriteAccess)
                    , TokenLiteral "sqrt"
                    , TokenSlot (SA s ReadAccess)
                    ]
    return result
expToCore (ECall "__touch" [e]) = do
    s <- expToCore e
    emit $ CoreTouch (SA s MutateAccess)
    return s
expToCore e = error $ "ICE: can't expToCore: " ++ show e

