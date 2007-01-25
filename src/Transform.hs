module Transform
(transform)
where

import Chunk
import Slot
import Data.Maybe
import AST
import Monad

transform = sequence . map trStmt

data Val s = Val {
    lvalue :: Maybe (Slot s),
    rvalue :: Maybe (Slot s),
    dep    :: [Chunk s]
    }

val dep = Val Nothing Nothing dep

trExp (EConst c) = do
    s <- newSlot
    constrainSlotType s $ typeOf c
    chunk <- newChunk [CConstAssign s c] []
    return $ (val [chunk]) { rvalue = Just s }

trExp (ELexical name) = do
    lex <- getLex name
    return $ (val []) { rvalue = Just lex, lvalue = Just lex }

trExp (EAssign e1 e2) = do
    dest <- trExp e1
    src  <- trExp e2
    when (isNothing $ lvalue dest) $ error "assignment with non-lvalue"
    chunk <- newChunk [CAssign (fromJust $ lvalue dest) (fromJust $ rvalue src)] (dep dest ++ dep src)
    return $ (val [chunk]) { rvalue = rvalue src }

trExp (EBinaryOp op e1 e2) = do
    v1 <- trExp e1
    v2 <- trExp e2
    slotTemp <- newSlot
    t1 <- getSlotType $ fromJust $ rvalue v1
    t2 <- getSlotType $ fromJust $ rvalue v2
    constrainSlotType slotTemp $ t1 `vand` t2
    
    chunk <- newChunk [CAssign slotTemp (fromJust $ rvalue v1),
                       CTokLine [CTokLit op, CTokSlot (SM slotTemp mchange), CTokSlot (SM (fromJust $ rvalue v2) mread)]]
                      (dep v1 ++ dep v2)
    return $ (val [chunk]) { rvalue = Just slotTemp }

-- XXX HACK
trExp (ECall "print" [exp]) = do
    v <- trExp exp
    -- XXX ANOTHER HACK
    constrainSlotType (fromJust $ rvalue v) vnumber
    chunk <- newChunk [CTokLine [CTokLit "outv", CTokSlot (SM (fromJust $ rvalue v) mread)]] (dep v)
    return $ val [chunk]

trStmt (SExpr exp) = do
    trExp exp
    dumpChunks
