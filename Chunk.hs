module Chunk
( newChunk, ChunkToken(..), ChunkLine(..), Chunk(..), debugTransformer,
  dumpChunks, Block(..), runTransform, findLex, getLex, newLex
)
where

import LiftST
import Control.Monad.ST
import Data.STRef
import Data.List
import Control.Monad.State
import Slot
import AST
import Utils

import qualified Data.Map as Map
import Data.Map (Map)

data Block s = InlineBlock [Chunk s]
             | BranchBlock [Block s]
             | ReentrantBlock [Block s]
             deriving (Show)

type ChunkTransform s = StateT (CTState s) (ST s)

data CTState s = CTState {
    nextSlotID  :: Int,
    nextChunkID :: Int,
    allChunks   :: [Chunk s],
    lexicals    :: Map String (Slot s)
    }

ctinit = CTState {
    nextSlotID  = 0,
    nextChunkID = 0,
    allChunks   = [],
    lexicals    = Map.empty
    }

findLex name = do
    s <- get
    return $ Map.lookup name (lexicals s)

getLex name = do
    lex <- findLex name
    case lex of
        Nothing -> newLex name
        Just l  -> return l

newLex name = do
    slot <- newSlot
    s <- get
    when (Map.member name (lexicals s)) $ error $ "ICE: variable name collision: " ++ name
    put $ s { lexicals = Map.insert name slot (lexicals s) }
    return slot

runTransform = flip evalStateT ctinit

dumpChunks = do
    s <- get
    put $ s { allChunks = [] }
    return $ InlineBlock (reverse $ allChunks s)

instance SlotStateT (ChunkTransform s) where
    newSlotID = do
        s <- get
        put $ s { nextSlotID = succ $ nextSlotID s }
        return $ nextSlotID s

newChunkID = do
    s <- get
    put $ s { nextChunkID = succ $ nextChunkID s }
    return $ nextChunkID s

data ChunkToken s = CTokLit  String
                  | CTokSlot (SlotMutation s)

data ChunkLine s  = CTokLine [ChunkToken s]
                  | CAssign  (Slot s) (Slot s)
                  | CConstAssign (Slot s) ConstValue

instance Show (ChunkToken s) where
    show (CTokLit s) = s
    show (CTokSlot sm) = show sm

instance Show (ChunkLine s) where
    show (CTokLine l) = Utils.join " " (map show l)
    show (CAssign s sm) = "#ASSIGN " ++ (show s) ++ " " ++ (show sm)
    show (CConstAssign s cv) = "#CASSIGN " ++ (show s) ++ " " ++ (show cv)

data Chunk s = Chunk {
    chunkID   :: Int,
    cSlots     :: [SlotMutation s],
    cTokens    :: [ChunkLine s],
    cDeps      :: [Chunk s],
    cRDeps     :: STRef s [Chunk s],
    cDepremain :: STRef s Int
    }

instance Ord (Chunk s) where
    c `compare` c' = (chunkID c) `compare` (chunkID c')

instance Eq (Chunk s) where
    c == c' = (chunkID c) == (chunkID c')

newChunk tokens deps = do
    id <- newChunkID
    let slots = findSlots tokens
    let deps' = pruneDeps deps
    rdeps <- liftST $ newSTRef []
    deprem <- liftST $ newSTRef (length deps')
    let new = Chunk {
        chunkID = id,
        cSlots = slots,
        cTokens = tokens,
        cDeps = deps',
        cRDeps = rdeps,
        cDepremain = deprem
        }
    fixRDeps new
    st <- get
    put $ st { allChunks = new:allChunks st }
    return new

fixRDeps chunk = do
        let deps = cDeps chunk
        sequence_ $ map fixup deps
    where
        fixup c' = liftST $ modifySTRef (cRDeps c') dofixup
        dofixup rdeps = pruneDeps $ chunk:rdeps
    
uniq [] = []
uniq [x] = [x]
uniq (a:b:t)
    | a == b
    = uniq (a:t)
    | otherwise
    = a:b:t

pruneDeps deps =
    uniq $ sort deps

findSlots tokens = mergeSlots $ sort (concat $ map scanLine tokens)
    where
        mergeSlots []  = []
        mergeSlots [x] = [x]
        mergeSlots (a@(SM as am):b@(SM bs bm):t)
            | (slotID as) == (slotID bs)
            = (SM as (am `mmerge` bm)):mergeSlots t
            | otherwise
            = a:mergeSlots (b:t)
        scanToken (CTokLit _) = []
        scanToken (CTokSlot s) = [s]
        scanLine (CTokLine l) = concat $ map scanToken l
        scanLine (CAssign s s') = [(SM s moverwrite), (SM s' mread)]
        scanLine (CConstAssign s _) = [(SM s moverwrite)]

dumpChunk c =
    "Chunk " ++ (show $ chunkID c) ++ "\n"
 ++ "  cSlots = { " ++ Utils.join ", " (map dumpMutation (cSlots c)) ++ " }\n"
 ++ "  cTokens = {\n" ++ show (cTokens c) ++ "\n  }\n"
 ++ "  cDeps = { " ++ Utils.join ", " (map (show . chunkID) (cDeps c)) ++ " }\n"
    where
        dumpMutation = show

instance Show (Chunk s) where
    show = dumpChunk
        
debugTransformer :: ChunkTransform s r -> ST s String
debugTransformer m = flip evalStateT ctinit $ do
    m
    st <- get
    return $ unlines (reverse $ map dumpChunk (allChunks st))
