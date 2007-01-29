module Kaos.CoreStorage (markStorage, Storage(..), SlotStorage, MarkedBlock, MarkedLine) where

import Kaos.Core
import Kaos.Slot
import Kaos.KaosM
import Data.List
import Data.Maybe
import Data.Generics
import Control.Monad.State hiding (State)
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

markLine (CoreLine tokens) = do
    fmap CoreLine . mapM markToken $ tokens
    where
        markToken (TokenLiteral l) = return $ TokenLiteral l
        markToken (TokenConst cv ) = return $ TokenConst  cv
        markToken (TokenSlot sa@(SA (s, future) WriteAccess)) = do
            storage <- newStorage s future
            return $ TokenSlot (SA (storage, s, future) WriteAccess)
        markToken (TokenSlot sa@(SA (s, future) acc)) = do
            storage <- getStorage s
            return $ TokenSlot (SA (storage, s, future) acc)
