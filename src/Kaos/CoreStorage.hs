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
module Kaos.CoreStorage (markStorage, Storage(..), StorageS(..), StorageMap, getSM) where

import Kaos.Core
import Kaos.Slot
import Kaos.KaosM
import Data.List
import Data.Maybe
import Data.Generics
import Control.Monad.State hiding (State)
import qualified Control.Monad.State as S
import Kaos.CoreFuture
--import Kaos.CoreAccess
import Kaos.VirtRegister
import Kaos.AST
import Control.Monad.Reader
import Kaos.Dump
import Kaos.CoreInline (inlineFallback)

import qualified Data.Map as M

data Storage    = Private VirtRegister
                | Shared  VirtRegister
                | Const   ConstValue
                | Phantom
                deriving (Show, Eq, Ord, Data, Typeable)

type StorageMap = M.Map Slot Storage
data StorageS = StorageS { ssStorage :: !StorageMap
                         , ssFuture  :: !FutureS
                         } deriving (Eq, Ord, Show, Data, Typeable)
type MarkM a = ReaderT FutureS (StateT StorageMap (VRegAllocT KaosM)) a

instance Futurable StorageS where getFuture = getFuture . ssFuture
instance LineAccess StorageS where getLineAccess = getLineAccess . ssFuture

getSM :: StorageS -> StorageMap
getSM = ssStorage

asksFuture :: (MonadReader f m, Futurable f)
           => (FutureMap -> a)
           -> m a
asksFuture f = asks (f . getFuture)

getStorage :: Slot -> MarkM (Maybe Storage)
getStorage = lift . gets . M.lookup
setStorage :: Storage -> Slot -> MarkM ()
setStorage st sl = lift $
    modify $ M.insert sl st
newStorage :: Slot -> Future -> MarkM ()
newStorage slot (Just (Bound b)) = setStorage (Private b) slot
newStorage slot _ = do
    vr <- lift . lift $ newVReg
    setStorage (Private vr) slot

markStorage :: Core FutureS -> KaosM (Core StorageS)
markStorage = runVRegAllocT . flip evalStateT M.empty . flip runReaderT undefined . markBlock

markBlock :: Core FutureS -> MarkM (Core StorageS)
markBlock (CB l) = saveCtx $ fmap CB $ mapM enterLine l
    where
        enterLine :: (CoreLine FutureS, FutureS) -> MarkM (CoreLine StorageS, StorageS)
        enterLine (line, future) = do
            debugDump "dump-storage-assignment" $ "MARKING:\n" ++ dumpCore (CB [(line, future)])
            line' <- local (const future) $ markLine line
            storage <- get
            debugDump "dump-storage-assignment" $ " => " ++ show storage
            return (line', StorageS storage future)

markLine :: CoreLine FutureS -> MarkM (CoreLine StorageS)
markLine l@(CoreNote (ContextNote ctx)) = do
    putCtx $ Just ctx
    return $ fmap undefined l
markLine l@(CoreNote _) = return $ fmap undefined l
markLine CoreTargZap = return CoreTargZap
markLine l@(CoreTouch sa) = do
    markLine $ CoreLine [ TokenSlot sa ]
    return $ fmap undefined l
markLine l@(CoreConst dest cv) = do
    future <- asksFuture (M.lookup dest)
    case future of
        Nothing -> return ()
        Just Read -> do
            setStorage (Const cv) dest
        Just (Bound r) -> do
            setStorage (Private r) dest
        _ -> do
            newStorage dest future
    return $ fmap undefined l

markLine l@(CoreAssign dest src) = do
    fdest <- asksFuture (M.lookup dest)
    fsrc  <- asksFuture (M.lookup  src)
    modify $ M.delete dest
    case (fdest, fsrc) of
        (Just (Bound b), _) -> do
            setStorage (Private b) dest
        (Nothing, _) -> return () -- unused
        (_, Nothing) -> do -- rename
            ssrc <- getStorage src
            modify $ M.alter (const ssrc) dest
            modify $ M.delete src
        (Just Read, Just Read) -> do -- alias
            ssrc <- getStorage src
            case ssrc of
                Nothing            -> fail "src storage was Nothing in markLine, alias case"
                (Just (Private r)) -> setStorage (Shared r) src
                _                  -> return ()
            ssrc' <- getStorage src
            setStorage (fromJust ssrc') dest
        (_, _) -> do -- copy
            newStorage dest fdest
    return $ fmap undefined l

markLine l@(CoreLine tokens) = do
    let collected = execState (mapM_ collect tokens) M.empty
    mapM_ updateStorage $ M.toList collected
    return $ fmap undefined l
    where
        collect :: CoreToken -> S.State (M.Map Slot AccessType) ()
        collect (TokenSlot (SA s access)) = do
            access' <- gets (fromMaybe NoAccess . M.lookup s)
            modify $ M.insert s (access' `mergeAccess` access)
        collect _ = return ()

        updateStorage (slot, WriteAccess) = do
            future <- asks (M.lookup slot . getFuture)
            newStorage slot future
        updateStorage _ = return ()

markLine l@(CoreLoop body) = do
    trueFuture <- asks getFuture
    future     <- setupFuture l trueFuture
    body'      <- liftK $ markBlockFuture future body
    body''     <- markBlock body'
    return $ CoreLoop body''
    where
        setupFuture :: CoreLine FutureS -> FutureMap -> MarkM FutureMap
        setupFuture _ trueFuture = do
            acc <- asks getLineAccess
            let mut = map fst $ filter ((== MutateAccess) . snd) (M.toList $ getAM acc)
            mutF <- mapM fixReg mut
            let future' = foldl updReg trueFuture mutF
            return future'
        updReg m (key, newVal) = M.alter (const newVal) key m
        fixReg slot = do
            stor <- getStorage slot
            case stor of
                Just (Private r) -> return (slot, Just $ Bound r)
                Nothing -> return (slot, Nothing)
                _ -> fail $ "Coreloop, bad fixreg storage " ++ show stor
        


markLine l@(CoreCond cond ontrue_ onfalse_) = do
    trueFuture <- ask
    future  <- setupFuture trueFuture
    ontrue  <- liftK $ (markBlockFuture future ontrue_)
    onfalse <- liftK $ (markBlockFuture future onfalse_)
    CoreLine cond' <- markLine (CoreLine cond)
    s <- get
    ontrue' <- markBlock ontrue
    s_t <- get
    put s
    onfalse' <- markBlock onfalse
    s_f <- get
    let u1 = s_f `M.union` s_t
    let u2 = s_t `M.union` s_f
    when (u1 /= u2) $ fail $ unlines["Storage states diverged:", dumpCoreLine (fmap (const()) l), dumpMap s_t, dumpMap s_f, dumpMap future]
    put u1
    return $ CoreCond cond' ontrue' onfalse'
    where
        setupFuture :: FutureS -> MarkM (M.Map Slot Lookahead)
        setupFuture trueFuture = do
            let acc = M.toList . getAM $ getLineAccess trueFuture
            let tf' = getFuture trueFuture
            entries <- fmap concat $ mapM (setupEntry tf') acc
            return $ M.fromList entries
        setupEntry future (s, acc) = do
            setupEntry' (s, acc) (M.lookup s future)
        -- need to bind read slots to ensure they aren't improperly aliased
--        setupEntry' (slot, ReadAccess) _ = return []
        setupEntry' _ Nothing            = return [] -- if we don't use it, it can go wherever (XXX: is this safe wrt underlying regalloc?)
        setupEntry' (slot, acc) future = do
            curAcc <- getStorage slot
            case curAcc of
                Just (Private r) -> return [(slot, Bound r)]
                Nothing -> do
                    newStorage slot future
                    Just (Private st) <- getStorage slot
                    return [(slot, Bound st)]
                x -> fail $ "trying to bind a shared slot: " ++ show (slot, x, acc, fmap (const ()) l)
        
markLine (CoreFoldable folder body) = do
    body' <- markLine body
    return $ CoreFoldable folder body'

markLine (CoreTargReader ts slot body) = do
    markLine (CoreAssign ts slot)
    markLine (CoreLine [TokenSlot (SA ts ReadAccess)])
    body' <- markBlock body
    return $ CoreTargReader ts slot body'

markLine (CoreTargWriter slot body) = do
    body' <- markBlock body
    markLine (CoreLine [TokenSlot (SA slot WriteAccess)])
    return $ CoreTargWriter slot body'

markLine (CoreInlineFlush l) = return $ CoreInlineFlush l
markLine l@(CoreInlineAssign level targUser dest repl) = do
    markLine $ inlineFallback l
    return $ CoreInlineAssign level targUser dest repl
