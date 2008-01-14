module Kaos.ASTToCore (astToCore) where

import Kaos.Core
import Kaos.AST
import Kaos.Slot
import Kaos.KaosM
import Control.Monad.Writer
import Kaos.Typecheck
import Data.Generics

type CoreWriter m a = WriterT [CoreLine ()] (TypeCheckT m) a

captureBlock :: MonadKaos m => CoreWriter m x -> CoreWriter m (CoreBlock ())
captureBlock = liftM (unitBlock . snd) . censor (const []) . listen

--XXX: should this be in Core?
unitBlock :: [CoreLine ()] -> CoreBlock ()
unitBlock = CB . map (\line -> (line, ()))

astToCore :: MonadKaos m => Statement Slot -> TypeCheckT m (Core ())
astToCore ast = do
    ((), ls) <- runWriterT $ astToCore' ast
    return $ unitBlock (ls :: [CoreLine ()])

astToCore' :: MonadKaos m => Statement Slot -> CoreWriter m ()
astToCore' (SExpr e) = do
    expToCore e
    return ()
astToCore' (SBlock st) = mapM_ astToCore' st
astToCore' (SCond be btrue bfalse) = do
    cond        <- evalCond be
    ctrue  <- captureBlock $ astToCore' btrue
    cfalse <- captureBlock $ astToCore' bfalse
    emit $ CoreCond cond ctrue cfalse
astToCore' (SDoUntil cond stmt) = do
    stmt' <- captureBlock $ do
        emit $ CoreLine [TokenLiteral "LOOP"]
        astToCore' stmt
        cond' <- evalCond cond
        emit $ CoreLine $ [TokenLiteral "UNTL"] ++ cond'
    emit $ CoreLoop stmt'

astToCore' (SICaos caosGroups) = do
    mapM_ emitILine caosGroups

emitILine :: MonadKaos m => InlineCAOSLine Slot -> CoreWriter m ()
emitILine (ICAssign v1 v2) = do
    expToCore (EAssign (ELexical v1) (ELexical v2))
    return ()

emitILine (ICConst v1 cv) = do
    expToCore (EAssign (ELexical v1) (EConst cv))
    return ()

emitILine (ICLine tl) =
    (emit . CoreLine) =<< mapM translateITok tl

emitILine (ICTargReader var body) = do
    bodyCore <- captureBlock $ mapM_ emitILine body
    emit $ CoreTargReader dummySlot var bodyCore

emitILine (ICTargWriter var body) = do
    bodyCore <- captureBlock $ mapM_ emitILine body
    emit $ CoreTargWriter var bodyCore


translateITok :: MonadKaos m => InlineCAOSToken Slot -> CoreWriter m CoreToken
translateITok (ICVar l at) = do
    slot <- expToCore (ELexical l)
    return $ TokenSlot (SA slot at)

translateITok (ICWord s) = return $ TokenLiteral s

emit :: MonadKaos m => CoreLine () -> CoreWriter m ()
emit x = tell [x]

evalCond :: MonadKaos m => BoolExpr Slot -> CoreWriter m [CoreToken]
evalCond = fmap condToCore . everywhereM (mkM eval)
    where
        eval :: MonadKaos m => BoolExpr Slot -> CoreWriter m (BoolExpr Slot)
        eval (BCompare cmp e1 e2) = do
            s1 <- expToCore e1
            s2 <- expToCore e2
            s1 `sameType` s2
            return $ BCompare cmp (ELexical s1) (ELexical s2)
        eval x = return x

condToCore :: BoolExpr Slot -> [CoreToken]
condToCore (BAnd e1 e2) = (condToCore e1) ++ [TokenLiteral "&&"] ++ cmpToCore e2
condToCore (BOr  e1 e2) = (condToCore e1) ++ [TokenLiteral "||"] ++ cmpToCore e2
condToCore e = cmpToCore e

cmpToCore :: BoolExpr Slot -> [CoreToken]
cmpToCore (BCompare cmp (ELexical e1) (ELexical e2)) = 
    [TokenSlot (SA e1 ReadAccess)
    ,TokenLiteral $ comparisonToCAOS cmp
    ,TokenSlot (SA e2 ReadAccess)
    ]
cmpToCore e = error $ "unexpected non-normal form: " ++ show e

expToCore :: MonadKaos m => Expression Slot -> CoreWriter m Slot
expToCore (EConst c) = do
    s <- newSlot
    s `typeIs` constType c
    emit $ CoreConst s c
    return s
expToCore (ELexical s) = return s
expToCore (EBinaryOp op e1 e2) = do
    s1   <- expToCore e1
    s2   <- expToCore e2
    dest <- newSlot
    s1 `sameType` s2
    s1 `sameType` dest
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
    s1 `sameType` s2
    emit $ CoreAssign s1 s2
    return s1
expToCore (ECall "print" []) = 
    return $ error "XXX: void return"
expToCore (ECall "print" (h:t)) = do
    s <- expToCore h
    emit $ CoreTypeSwitch s (pt "outv" s) (pt "outs" s) pf
    expToCore (ECall "print" t)
    where
        pt verb slot = CoreLine [TokenLiteral verb, TokenSlot (SA slot ReadAccess)]
        pf = CoreLine [TokenLiteral "outs", TokenConst (CString "<object>")]
expToCore (ECall "sqrt" [e]) = do
    s <- expToCore e
    result <- newSlot
    s `sameType` result
    result `typeIs` typeNum
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

expToCore (EBoolCast c) = do
    c' <- evalCond c
    s  <- newSlot
    s `typeIs` typeNum

    emit $ CoreLine $
        [ TokenLiteral "doif" ] ++ c' ++
        [ TokenLiteral "setv", TokenSlot (SA s WriteAccess), TokenConst (CInteger 1) ] ++
        [ TokenLiteral "else" ] ++
        [ TokenLiteral "setv", TokenSlot (SA s WriteAccess), TokenConst (CInteger 0) ] ++
        [ TokenLiteral "endi" ]
    return s

expToCore e = error $ "ICE: can't expToCore: " ++ show e

