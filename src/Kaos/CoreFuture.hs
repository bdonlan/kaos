module Kaos.CoreFuture (markFuture, Lookahead(..), Future) where

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

markFuture :: Core Slot -> KaosM (Core (Slot, Future))
markFuture = flip evalStateT M.empty . markBlock

markBlock :: Core Slot -> FutureM (Core (Slot, Future))
markBlock = fmap reverse . mapM markLine . reverse

markLine :: CoreLine Slot -> FutureM (CoreLine (Slot, Future))
markLine (CoreTypeSwitch _ _ _ _) = fail "late CoreTypeSwitch (TODO: extra translate stage)"
markLine (CoreNote t) = return $ CoreNote t
markLine (CoreTouch sa@(SA t acc)) = do
    f <- getFuture t
    markLine $ CoreLine [ TokenSlot sa ]
    return $ CoreTouch (SA (t, f) acc)
markLine (CoreConst dest cv) = do
    f <- getFuture dest
    modify $ M.delete dest
    return $ CoreConst (dest, f) cv
markLine line@(CoreLine tokens) = do
    let mergeAcc = lineAccess line
    tok' <- mapM markToken tokens
    mapM_ update mergeAcc
    return $ CoreLine tok'
    where
        markToken (TokenConst c)       = return $ TokenConst c
        markToken (TokenLiteral s)     = return $ TokenLiteral s
        markToken (TokenSlot (SA s a)) = do
            future <- gets (M.lookup s)
            return $ TokenSlot (SA (s, future) a)
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
    return $ CoreAssign (dest, fdest) (src, fsrc)
            
markLine (CoreConst dest cv) = do
    fdest <- getFuture dest
    modify $ M.delete dest
    return $ CoreConst (dest, fdest) cv
