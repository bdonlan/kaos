module Kaos.CoreFuture (
    markFuture, markBlockFuture, Lookahead(..), FutureS, Future, lineAccess
    ) where

import Kaos.Core
import Kaos.Slot
import Kaos.KaosM
import Data.List
import Data.Maybe
import Data.Generics
import Control.Monad.State hiding (State)
import Kaos.VirtRegister
import Debug.Trace

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

markBlockFuture' :: FutureS -> Core a -> KaosM (Core FutureS, FutureS)
markBlockFuture' assumedFuture =
    flip runStateT assumedFuture . markBlock . fmap (const ())


markBlockFuture :: FutureS -> Core a -> KaosM (Core FutureS)
markBlockFuture assumedFuture = fmap fst . markBlockFuture' assumedFuture

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

markLine line = do
    mergeAcc <- liftK $ lineAccess line
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








lineAccess :: CoreLine t -> KaosM [(Slot, AccessType)]
lineAccess l = do
    la' <- lineAccess' l
    return $ M.toList $ foldl addAccess M.empty la'
    where
        addAccess m (SA s ac) = M.alter updateKey s m
            where
            updateKey Nothing = Just ac
            updateKey (Just a') = Just $ a' `mergeAccess` ac

lineAccess' :: CoreLine f -> KaosM [GenAccess Slot]
lineAccess' (CoreLine tokens) = return $ concatMap findAccess tokens
    where
        findAccess (TokenSlot s) = [s]
        findAccess _ = []
lineAccess' (CoreAssign s1 s2) = return $ 
    [SA s1 WriteAccess, SA s2 ReadAccess]
lineAccess' (CoreConst s1 _) = return [SA s1 WriteAccess]
lineAccess' (CoreCond condition ifTrue ifFalse)
    = do
        condLA <- lineAccess' $ CoreLine condition
        ma     <- mergedAccess
        return $ condLA ++ map snd ma
    where
        mergedAccess = do
            ifTrue'  <- buildMap ifTrue
            ifFalse' <- buildMap ifFalse
            return $ M.toList
                        $ M.mapWithKey finishMerge
                        $ M.unionWith (\(a, _) (b, _) -> (a, b))
                          ifTrue' ifFalse'
        buildMap :: CoreBlock a -> KaosM (M.Map Slot (AccessType, AccessType))
        buildMap = liftM (M.map (\a -> (a, NoAccess))) . blockAccess
        finishMerge slot (br1, br2) = SA slot (br1 `comb` br2)
        comb x y | x == y         = x
        comb x y
            | MutateAccess `elem` [x, y]
            = MutateAccess
        comb NoAccess WriteAccess = MutateAccess
        comb NoAccess ReadAccess  = ReadAccess
        comb x y                  = comb y x

lineAccess' (CoreLoop body) = do
    bodyAccess <- blockAccess body
    bodyFutures <- fmap snd $ markBlockFuture' M.empty body 
    let bodyAcc' = M.toList $ M.mapWithKey (merge bodyFutures) bodyAccess
    debugKM $ unlines $ map (\k -> show (k, M.lookup k bodyFutures :: Future, M.lookup k (M.fromList bodyAcc') :: Maybe AccessType)) (map fst bodyAcc')
    return $ map (uncurry SA) (bodyAcc')
    where
        merge future slot access = merge' (M.lookup slot future) access
        merge' Nothing      ReadAccess  = ReadAccess
        merge' Nothing      _           = WriteAccess
        merge' (Just Read)  ReadAccess  = ReadAccess
        merge' x            y           = trace ("merge' fallback: " ++ show (x, y)) MutateAccess
        merge' _            _           = MutateAccess
lineAccess' _ = return []

blockAccess (CB b) = do
    las <- mapM (\(line, _) -> fmap M.fromList $ lineAccess line) b
    return $ M.unionsWith mergeAccess las
