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
module Kaos.ASTToCore (astToCore) where

import Kaos.Core
import Kaos.AST
import Kaos.Slot
import Kaos.KaosM
import Control.Monad.Writer
import Data.Generics
import Data.Maybe
import Data.Bits
import qualified Data.Map as M

newtype CoreWriter m a = CW { unCW :: WriterT [CoreLine ()] m a }
    deriving (Monad, MonadKaos, KaosDiagM, Functor)

instance KaosDiagM m => MonadWriter [CoreLine ()] (CoreWriter m) where
    tell m = CW $ do
        ctx <- getCtx
        when (isJust ctx) $ tell [CoreNote $ ContextNote (fromJust ctx)]
        tell m
    listen = CW . listen . unCW
    pass = CW . pass . unCW

typeIs :: KaosDiagM m => Slot -> CAOSType -> m ()
typeIs (Slot _ _ ct') ct
    | ct `typeMatches` ct'
    = return ()
    | otherwise
    = compileError "Type mismatch"

sameType :: KaosDiagM m => Slot -> Slot -> m ()
sameType s1 s2 = typeIs s1 (slotType s2)

captureBlock :: KaosDiagM m => CoreWriter m x -> CoreWriter m (CoreBlock ())
captureBlock = liftM (unitBlock . snd) . censor (const []) . listen

--XXX: should this be in Core?
unitBlock :: [CoreLine ()] -> CoreBlock ()
unitBlock = CB . map (\line -> (line, ()))

astToCore :: KaosDiagM m => Statement Slot -> m (Core ())
astToCore ast = do
    ((), ls) <- runWriterT $ unCW (astToCore' ast)
    return $ unitBlock (ls :: [CoreLine ()])

astToCore' :: KaosDiagM m => Statement Slot -> CoreWriter m ()
astToCore' (SContext c s) = context c $ astToCore' s
astToCore' s@(SDeclare _ _) = fail $ "Late SDeclare: " ++ show s
astToCore' (SExpr e) = do
    expToCore e
    return ()
astToCore' (SBlock st) = mapM_ astToCore' st
astToCore' (SCond be btrue bfalse) = do
    cond   <- evalCond be
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

astToCore' (SUntil cond stmt) = do
    slotTemp <- newSlot typeNum
    emit $ CoreConst slotTemp (CInteger 1)

    let ifWrap = SCond  (BCompare CEQ (ELexical slotTemp) (EConst $ CInteger 1))
                        stmt
                        (SExpr (EAssign (ELexical slotTemp) (EConst $ CInteger 1)))
    let newLoop = SDoUntil cond ifWrap
    astToCore' newLoop
   
astToCore' (SICaos caosGroups) = do
    mapM_ emitILine caosGroups
astToCore' (SInstBlock stmt) = do
    emit $ CoreLine [TokenLiteral "INST"]
    astToCore' stmt
    emit $ CoreLine [TokenLiteral "SLOW"]
astToCore' (SIterCall _ _ _ _) = fail "late SIterCall"
astToCore' (SFlush level) = emit $ CoreInlineFlush level
astToCore' (SScriptHead ex) = do
    sx <- mapM expToCore ex
    mapM_ (`typeIs` typeNum) sx
    emit $ CoreLine ([TokenLiteral "SCRP"] ++ (map TokenConstSlot sx))

emitILine :: KaosDiagM m => InlineCAOSLine Slot -> CoreWriter m ()
emitILine (ICAssign v1 v2) = do
    expToCore (EAssign (ELexical v1) (ELexical v2))
    return ()

emitILine (ICConst v1 cv) = do
    expToCore (EAssign (ELexical v1) (EConst cv))
    return ()

emitILine (ICLine tl) =
    (emit . CoreLine) =<< mapM translateITok tl

emitILine (ICTargReader var body) = do
    var `typeIs` typeObj
    bodyCore <- captureBlock $ mapM_ emitILine body
    emit $ CoreTargReader dummySlot var bodyCore

emitILine (ICTargWriter var body) = do
    var `typeIs` typeObj
    bodyCore <- captureBlock $ mapM_ emitILine body
    emit $ CoreTargWriter var bodyCore

emitILine (ICLoop body) = do
    bodyCore <- captureBlock $ mapM_ emitILine body
    emit $ CoreLoop bodyCore

emitILine (ICKaos st) = astToCore' st
emitILine (ICLValue level slot tokens) = do
    tokens' <- mapM translateITok tokens
    -- ciaTargUser set later
    emit $ CoreInlineAssign level False slot tokens'
emitILine ICTargZap = emit CoreTargZap

translateITok :: KaosDiagM m => InlineCAOSToken Slot -> CoreWriter m CoreToken
translateITok (ICVar l at) = do
    slot <- expToCore (ELexical l)
    return $ TokenSlot (SA slot at)

translateITok (ICWord s) = return $ TokenLiteral s

emit :: KaosDiagM m => CoreLine () -> CoreWriter m ()
emit x = tell [x]

evalCond :: KaosDiagM m => BoolExpr Slot -> CoreWriter m [CoreToken]
evalCond = fmap condToCore . everywhereM (mkM eval)
    where
        eval :: KaosDiagM m => BoolExpr Slot -> CoreWriter m (BoolExpr Slot)
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

type FolderCore = ConstValue -> ConstValue -> Maybe ConstValue

numFolder   :: (Double -> Double -> Double)
            -> (Int -> Int -> Int)
            -> FolderCore
numFolder floatOp intOp v1 v2 = Just $ mathOp v1 v2
    where
        promote (CInteger i1) = CFloat $ fromIntegral i1
        promote v@(CFloat _) = v
        promote x = error $ "Bad type in constant folding: " ++ show x
        mathOp (CFloat f1) (CFloat f2) = CFloat $ floatOp f1 f2
        mathOp (CInteger i1) (CInteger i2) = CInteger $ intOp i1 i2
        mathOp a b = mathOp (promote a) (promote b)

binaryFolder :: Maybe FolderCore -> Slot -> Slot -> Slot -> Folder
binaryFolder Nothing _ _ _ _ = Nothing
binaryFolder (Just op) dest s1 s2 varlookup
    =   case (varlookup s1, varlookup s2) of
            (Just v1, Just v2) ->
                case (op v1 v2) of
                    Just ret -> Just [CoreConst dest ret]
                    Nothing  -> Nothing
            _ -> Nothing

numBinOp    :: String
            -> (Double -> Double -> Double)
            -> (Int -> Int -> Int)
            -> ( (CAOSType, CAOSType), BinOp)
numBinOp token dfold ifold =
    ( (typeNum, typeNum), BO token typeNum (Just $ numFolder dfold ifold))


intBinOp    :: String
            -> (Int -> Int -> Int)
            -> ( (CAOSType, CAOSType), BinOp)
intBinOp token ifold =
    ( (typeNum, typeNum), BO token typeNum (Just doFold) )
    where
        doFold (CInteger v1) (CInteger v2) = Just $ CInteger $ ifold v1 v2
        doFold _ _ = Nothing

data BinOp = BO String CAOSType (Maybe FolderCore)
binaryOps :: M.Map String (M.Map (CAOSType, CAOSType) BinOp)
binaryOps = M.fromList $ map (\(n, l) -> (n, M.fromList l)) binaryOps'

binaryOps' :: [(String, [((CAOSType, CAOSType), BinOp)])]
binaryOps' = [
    ("addv", [numBinOp "addv" (+) (+)]),
    ("subv", [numBinOp "subv" (-) (-)]),
    ("mulv", [numBinOp "mulv" (*) (*)]),
    ("divv", [numBinOp "divv" (/) div]),
    ("andv", [intBinOp "andv" (.&.)]),
    ("orrv", [intBinOp "orrv" (.|.)])
    ]

expToCore :: KaosDiagM m => Expression Slot -> CoreWriter m Slot
expToCore (EConst c) = do
    s <- newSlot $ constType c
    emit $ CoreConst s c
    return s
expToCore (ELexical s) = return s
expToCore (EBinaryOp opName e1 e2) = do
    s1   <- expToCore e1
    s2   <- expToCore e2
    let t1 = slotType s1
    let t2 = slotType s2
    op <- M.lookup opName binaryOps -- shouldn't fail
    case M.lookup (t1, t2) op of
        Nothing -> compileError "Type mismatch in binary operation"
        Just (BO caosTok retType folderF) -> do
            dest <- newSlot retType
            emit $ CoreAssign dest s1
            emit $ CoreFoldable (binaryFolder folderF dest s1 s2)
                 $ CoreLine [ TokenLiteral caosTok
                            , TokenSlot (SA dest MutateAccess)
                            , TokenSlot (SA s2   ReadAccess)
                            ]
            return dest

expToCore (EAssign e1 e2) = do
    -- TODO: determine if e1 is mutable
    s1 <- expToCore e1
    s2 <- expToCore e2
    s1 `sameType` s2
    emit $ CoreAssign s1 s2
    return s1
expToCore (ECall "anim" (obj:nums)) = do
    args <- sequence $ zipWith checkArgs nums [1..]
    os <- expToCore obj
    os `typeIs` typeObj
    let l = h ++ args ++ t
    emit $ CoreTargReader dummySlot os (CB $ [(CoreLine l, ())])
    return dummySlot
    where
        h = [TokenLiteral "anim", TokenLiteral "["]
        t = [TokenLiteral "]"]
        checkArgs (EConst c@(CInteger i)) n
            | i < 0 || i > 255
            = compileError $ "Argument " ++ (show n) ++ " of anim is out of range; " ++ (show i) ++ " is not between 0 and 255"
            | otherwise
            = return $ TokenConst c
        checkArgs _ n = compileError $ "Argument " ++ (show n) ++ " of anim is invalid; must be a constant integer."

expToCore (ECall "print" []) = 
    return $ error "XXX: void return"
expToCore (ECall "print" (x:xs)) = do
    s <- expToCore x
    verb <- getVerb (slotType s)
    emit $ CoreLine [TokenLiteral verb, TokenSlot (SA s ReadAccess)]
    expToCore (ECall "print" xs)
    where
        getVerb t
            | t == typeNum
            = return "outv"
            | t == typeStr
            = return "outs"
            | otherwise
            = compileError "Bad type for print"
expToCore (ECall "__touch" [e]) = do
    s <- expToCore e
    emit $ CoreTouch (SA s MutateAccess)
    return s
expToCore (ECall "__const" [e]) = do
    s <- expToCore e
    let t = slotType s
    s' <- newSlot t
    v <- assignVerb t
    emit $ CoreLine [TokenLiteral v, TokenSlot (SA s' WriteAccess), TokenConstSlot s]
    return s'
    where
        assignVerb t
            | t == typeNum
            = return "setv"
            | t == typeStr
            = return "sets"
            | t == typeObj
            = return "seta"
            | otherwise
            = compileError "bad type for assign in __const"

expToCore (ECall s _) = compileError $ "Unknown macro or builtin " ++ show s

expToCore (EBoolCast c) = do
    c' <- evalCond c
    s  <- newSlot typeNum

    emit $ CoreLine $
        [ TokenLiteral "doif" ] ++ c' ++
        [ TokenLiteral "setv", TokenSlot (SA s WriteAccess), TokenConst (CInteger 1) ] ++
        [ TokenLiteral "else" ] ++
        [ TokenLiteral "setv", TokenSlot (SA s WriteAccess), TokenConst (CInteger 0) ] ++
        [ TokenLiteral "endi" ]
    return s

expToCore (EStmt slot code) = do
    astToCore' code
    case slot of
        Just s  -> return s
        Nothing -> do
            s <- newSlot typeVoid
            return s
