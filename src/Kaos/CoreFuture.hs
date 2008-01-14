module Kaos.CoreFuture (
    markFuture, markBlockFuture, markBlockFuture',
    Lookahead(..), Future,
    Futurable(..), lineFuture, FutureMap, FutureS
    ) where

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
               deriving (Show, Eq, Ord, Data, Typeable)

type FutureMap = M.Map Slot Lookahead
data FutureS = FutureS { fsFuture ::  FutureMap
                       , fsAccess :: !AccessMap
                       } deriving (Eq, Ord, Show, Data, Typeable)
type Future  = Maybe Lookahead

class Futurable a where getFuture :: a -> FutureMap
instance Futurable FutureS where getFuture = fsFuture
instance LineAccess FutureS where getLineAccess = fsAccess

lineFuture :: Futurable t => (CoreLine t, t) -> FutureMap
lineFuture = getFuture . snd

type FutureM a = StateT FutureMap KaosM a

lookupFuture :: Slot -> FutureM Future
lookupFuture = gets . M.lookup

markFuture :: LineAccess t => Core t -> KaosM (Core FutureS)
markFuture = markBlockFuture M.empty

markBlockFuture' ::
    FutureMap -> Core AccessMap -> KaosM (Core FutureS, FutureMap)
markBlockFuture' assumedFuture =
    flip runStateT assumedFuture . markBlock


markBlockFuture ::
    LineAccess am => FutureMap -> Core am -> KaosM (Core FutureS)
markBlockFuture assumedFuture =
    fmap fst . markBlockFuture' assumedFuture . fmap getLineAccess

markBlock :: Core AccessMap -> FutureM (Core FutureS)
markBlock (CB l) = do
    ls <- mapM markLine_ (reverse l)
    return $ CB (reverse ls)
    where
        markLine_ :: (CoreLine AccessMap, AccessMap) -> FutureM (CoreLine FutureS, FutureS)
        markLine_ (line, acc) = do
            future <- get
            markLine line acc
            return (fmap (zotFuture line) line, FutureS future acc)
        zotFuture line am = FutureS (err line) am
        err line = error $ "Tried to use the future of a nested line: " ++ show line

markLine :: CoreLine AccessMap -> AccessMap -> FutureM ()
markLine (CoreTypeSwitch _ _ _ _) _ = fail "late CoreTypeSwitch (TODO: extra translate stage)"
markLine (CoreNote _) _ = return ()
markLine (CoreTouch sa) accM = do
    markLine (CoreLine [ TokenSlot sa ]) accM
markLine (CoreConst dest _) _ =
    modify $ M.delete dest

markLine (CoreAssign dest src) _ = do
    fsrc  <- lookupFuture src
    fdest <- lookupFuture dest
    modify $ M.delete dest
    case (fsrc, fdest) of
        (Nothing, Nothing) -> return () -- discard
        (Just Read, Just Read) -> return () -- share
        (Nothing, _) -> do -- rename
            modify $ M.alter (const fdest) src
        (_, _) -> return () -- overwrite; we don't set future as it's already non-Nothing

markLine _ acc = do
    mapM_ update (M.toList $ getAM acc)
    where
        update :: (Slot, AccessType) -> FutureM ()
        update (s, WriteAccess)  = modify $ M.delete s
        update (s, ReadAccess)   = modify $ flip M.alter s (`mplus` Just Read)
        update (s, MutateAccess) = modify . flip M.alter s $ \st ->
            case st of
                Just (Bound _) -> st
                _              -> Just Mutate
        update (_, NoAccess)     = return ()







{-
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
-}
