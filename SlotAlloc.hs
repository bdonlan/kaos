module SlotAlloc (allocSlots, runSlotAlloc, RToken(..), RBlock(..)) where

import Register
import Control.Monad.ST
import Control.Monad.State
import Chunk
import Slot
import AST
import LiftST
import Debug.Trace

class SlotAllocator m where
    nextRegID :: m Int

newReg = do
    rid <- nextRegID
    return $ R rid

allocReg slot = do
    reg <- newReg
    setReg slot reg
    return reg

data RToken s =  RTLiteral String
               | RTRegister (Register s)
               deriving (Show, Eq, Ord)

data RBlock s =  RLine  [RToken s]
               | RBlock [RBlock s]
               deriving (Show, Eq, Ord)

data SAState = SAState {
    nextReg :: Int
    }

istate = SAState { nextReg = 0 }

runSlotAlloc m = evalStateT m istate

type SAMonad s = StateT SAState (ST s)

instance SlotAllocator (SAMonad s) where
    nextRegID = do
        s <- get
        put $ s { nextReg = succ $ nextReg s }
        return $ nextReg s

allocSlots = flattenSlots

-- return [SABlock]
flattenSlots l = do
    r <- sequence $ map flattenBlock l
    return $ concat r

-- return [SABlock]
flattenBlock (InlineBlock chunks) = do
    l <- sequence $ map flattenChunk chunks
    return $ concat l

-- return [SABlock]
flattenBlock (BranchBlock blocks) = flattenSlots blocks

flattenBlock (ReentrantBlock blocks) = do
    r <- flattenSlots blocks
    return [RBlock r]

flattenChunk c = do
    lines <- sequence $ map flattenLine (cTokens c)
    return $ map RLine lines

flattenLine (CTokLine line) = do
    tokens <- sequence $ map flattenToken line
    return tokens

flattenLine e@(CAssign s1 s2) = do
    t1 <- getSlotType s1
    t2 <- getSlotType s2
    t  <- linkSlotTypes s1 s2
    let assign = assignForType t
    r1 <- getRegRef s1
    r2 <- getRegRef s2
    return $ [RTLiteral assign, RTRegister r1, RTRegister r2]

flattenLine e@(CConstAssign s v) = do
    constrainSlotType s (typeOf v)
    let assign = assignForType (typeOf v)
    r <- getRegRef s
    return $ [RTLiteral assign, RTRegister r, flattenConst v]
 
flattenToken (CTokLit s) = return $ RTLiteral s
flattenToken (CTokSlot (SM s _)) = do
    rr <- getRegRef s
    return $ RTRegister rr

flattenConst (CString  s) = RTLiteral s
flattenConst (CInteger i) = RTLiteral (show i)
flattenConst (CFloat   f) = RTLiteral (show f)

getRegRef slot = do
    r <- getReg slot
    r' <- case r of
        Nothing -> allocReg slot
        Just r -> return r
    return r'
