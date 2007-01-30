module Kaos.CoreFuture (markFuture, markBlockFuture, Lookahead(..), FutureS, Future) where

import Kaos.Core
import Kaos.Slot
import Kaos.KaosM
import Data.List
import Data.Maybe
import Data.Generics
import Control.Monad.State hiding (State)
import Kaos.VirtRegister

import qualified Data.Map as M

data Lookahead = Bound VirtRegister
               | Read
               | Mutate
               deriving (Show, Eq, Data, Typeable)
type FutureS = M.Map Slot Lookahead
type Future  = Maybe Lookahead


type FutureM a = StateT FutureS KaosM a

getFuture = gets . M.lookup

markFuture :: Core a -> KaosM (Core FutureS)
markFuture = markBlockFuture M.empty

markBlockFuture :: FutureS -> Core a -> KaosM (Core FutureS)
markBlockFuture assumedFuture =
    flip evalStateT assumedFuture . markBlock . fmap (const ())

markBlock :: Core () -> FutureM (Core FutureS)
markBlock (CB l) = do
    lines <- mapM markLine_ (reverse l)
    return $ CB (reverse lines)
    where
        markLine_ :: (CoreLine (), ()) -> FutureM (CoreLine FutureS, FutureS)
        markLine_ (line, _) = do
            future <- get
            markLine line
            return (fmap (const $ err line) line, future)
        err line = error $ "Tried to use the future of a nested line: " ++ show line

markLine :: CoreLine () -> FutureM ()
markLine (CoreTypeSwitch _ _ _ _) = fail "late CoreTypeSwitch (TODO: extra translate stage)"
markLine (CoreNote t) = return ()
markLine (CoreTouch sa@(SA t acc)) = do
    markLine $ CoreLine [ TokenSlot sa ]
markLine (CoreConst dest cv) =
    modify $ M.delete dest
markLine line@(CoreLine tokens) = do
    let mergeAcc = lineAccess line
    mapM_ update mergeAcc
    where
        update :: (Slot, AccessType) -> FutureM ()
        update (s, WriteAccess)  = modify $ M.delete s
        update (s, ReadAccess)   = modify $ flip M.alter s (`mplus` Just Read)
        update (s, MutateAccess) = modify . flip M.alter s $ \st ->
            case st of
                Just (Bound _) -> st
                _              -> Just Mutate
        update (s, NoAccess)     = return ()

markLine line@(CoreCond cond _ _) = do
    let mergeAcc = lineAccess line
    mapM_ update mergeAcc
    where
        -- XXX this is a copy of CoreLine above, find a way to merge the two
        update :: (Slot, AccessType) -> FutureM ()
        update (s, WriteAccess)  = modify $ M.delete s
        update (s, ReadAccess)   = modify $ flip M.alter s (`mplus` Just Read)
        update (s, MutateAccess) = modify . flip M.alter s $ \st ->
            case st of
                Just (Bound _) -> st
                _              -> Just Mutate
        update (s, NoAccess)     = return ()
   
markLine (CoreAssign dest src) = do
    fsrc  <- getFuture src
    fdest <- getFuture dest
    modify $ M.delete dest
    case (fsrc, fdest) of
        (Nothing, Nothing) -> return () -- discard
        (Just Read, Just Read) -> return () -- share
        (Nothing, _) -> do -- rename
            modify $ M.alter (const fdest) src
        (_, _) -> return () -- overwrite; we don't set future as it's already non-Nothing
