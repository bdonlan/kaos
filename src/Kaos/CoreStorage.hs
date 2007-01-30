module Kaos.CoreStorage (markStorage, Storage(..), SlotStorage, MarkedBlock, MarkedLine) where

import Kaos.Core
import Kaos.Slot
import Kaos.KaosM
import Data.List
import Data.Maybe
import Data.Generics
import Control.Monad.State hiding (State)
import qualified Control.Monad.State as S
import Kaos.CoreFuture
import Kaos.VirtRegister
import Kaos.AST

import qualified Data.Map as M

data Storage    = Private VirtRegister
                | Shared  VirtRegister
                | Const   ConstValue
                | Phantom
                deriving (Show, Eq, Data, Typeable)

type SlotStorage = Maybe Storage

type MarkState = M.Map Slot Storage
type MarkM a = StateT MarkState (VRegAllocT KaosM) a

type MarkedBlock = Core     (SlotStorage, Slot, Future)
type MarkedLine  = CoreLine (SlotStorage, Slot, Future)


getStorage = gets . M.lookup
setStorage st sl = do
    modify $ M.insert sl st
    return $ Just st
setStorage_ st sl = setStorage st sl >> return ()
newStorage slot (Just (Bound b)) = setStorage (Private b) slot
newStorage slot _ = do
    vr <- lift newVReg
    setStorage (Private vr) slot

markStorage :: Core (Slot, Future) -> KaosM (Core (SlotStorage, Slot, Future))
markStorage = runVRegAllocT . flip evalStateT M.empty . markBlock

markBlock :: Core (Slot, Future) -> MarkM (Core (SlotStorage, Slot, Future))
markBlock = mapM markLine

markLine :: CoreLine (Slot, Future) -> MarkM (CoreLine (SlotStorage, Slot, Future))
markLine (CoreTypeSwitch _ _ _ _) = fail "late CoreTypeSwitch"
markLine (CoreNote t) = return $ CoreNote t
markLine (CoreTouch sa@(SA (slot, f) acc)) = do
    markLine $ CoreLine [ TokenSlot sa ]
    s <- getStorage slot
    return (CoreTouch (SA (s, slot, f) acc))
markLine (CoreConst (dest, future) cv) = do
    case future of
        Nothing -> return $ CoreConst (Nothing, dest, future) cv
        Just Read -> do
            st <- setStorage (Const cv) dest
            return $ CoreConst (st, dest, future) cv
        Just (Bound r) -> do
            st <- setStorage (Private r) dest
            return $ CoreConst (st, dest, future) cv
        _ -> do
            st <- newStorage dest future
            return $ CoreConst (st, dest, future) cv
markLine (CoreAssign (dest, fdest) (src, fsrc)) = do
    modify $ M.delete dest
    case (fdest, fsrc) of
        (Just (Bound b), _) -> do
            setStorage (Private b) dest
            return ()
        (Nothing, _) -> return () -- unused
        (_, Nothing) -> do -- rename
            ssrc <- getStorage src
            modify $ M.alter (const ssrc) dest
            modify $ M.delete src
        (Just Read, Just Read) -> do -- alias
            ssrc <- getStorage src
            case ssrc of
                Nothing            -> fail "src storage was Nothing in markLine, alias case"
                (Just (Private r)) -> setStorage_ (Shared r) src
                _                  -> return ()
            ssrc <- getStorage src
            setStorage (fromJust ssrc) dest
            return ()
        (_, _) -> do -- copy
            newStorage dest fdest
            return ()
    ssrc  <- getStorage src
    sdest <- getStorage dest
    return $ CoreAssign (sdest, dest, fdest) (ssrc, src, fsrc)

markLine l@(CoreLine tokens) = do
    mapM_ updateStorage $
        M.toList $ flip execState M.empty $ mapM_ collect tokens
    fmap CoreLine . mapM markToken $ tokens
    where
        collect :: CoreToken (Slot, Future)
                -> S.State (M.Map Slot (Future, AccessType)) ()
        collect (TokenSlot sa@(SA (s, future) access)) = do
            (_, access') <- gets (fromMaybe (undefined, NoAccess) . M.lookup s)
            modify $ M.insert s (future, access' `mergeAccess` access)
        collect _ = return ()

        updateStorage (slot, (future, WriteAccess)) = do
            newStorage slot future
            return ()
        updateStorage _ = return ()

        markToken (TokenLiteral l) = return $ TokenLiteral l
        markToken (TokenConst cv ) = return $ TokenConst  cv
        markToken (TokenSlot sa@(SA (s, future) acc)) = do
            storage <- getStorage s
            return $ TokenSlot (SA (storage, s, future) acc)

markLine l@(CoreCond cond _ _) = do
    -- We use the usage information from the defutured variant to determine
    -- what variables to pin
    let l'@(CoreCond _ ontrue_ onfalse_) = fmap fst l
    future  <- setupFuture l'
    ontrue  <- lift . lift $ evalStateT (markBlockFuture ontrue_)  future
    onfalse <- lift . lift $ evalStateT (markBlockFuture onfalse_) future
    CoreLine cond' <- markLine (CoreLine cond)
    s <- get
    ontrue' <- markBlock ontrue
    s_t <- get
    put s
    onfalse' <- markBlock onfalse
    s_f <- get
    let u1 = s_f `M.union` s_t
    let u2 = s_t `M.union` s_f
    when (u1 /= u2) $ fail $ "Storage states diverged: " ++ show (l, s_t, s_f, future)
    put u1
    return $ CoreCond cond' ontrue' onfalse'
    where
        setupFuture :: CoreLine Slot -> MarkM (M.Map Slot Lookahead)
        setupFuture l = do
            let acc = lineAccess l
            entries <- fmap concat $ mapM setupEntry acc
            return $ M.fromList entries
        setupEntry :: (Slot, AccessType) -> MarkM [(Slot, Lookahead)]
        setupEntry (slot, ReadAccess) = return []
        setupEntry (slot, acc) = do
            curAcc <- getStorage slot
            case curAcc of
                Just (Private r) -> return [(slot, Bound r)]
                Nothing -> do
                    Just (Private st) <- newStorage slot (Just Mutate) -- XXX: We may need to be bound here!
                    return [(slot, Bound st)]
                x -> fail $ "trying to bind a shared slot: " ++ show (x, acc, l)
        setupEntry _ = return []
        
