module Kaos.CoreAccess (markAccess) where

import Kaos.Core
import Kaos.Slot
import qualified Data.Map as M
import Data.Monoid
import Data.Generics
import Kaos.CoreFuture
import Debug.Trace
import Kaos.KaosM
import Control.Monad
import Kaos.AST

import Data.Generics.Basics
import Data.Generics.Aliases
-- | Monadic variation on everywhere'
everywhereM' :: Monad m => GenericM m -> GenericM m

-- Top-down order is also reflected in order of do-actions
everywhereM' f x = gmapM (everywhereM' f) =<< (f x)

dummyAccess :: CoreLine AccessMap -> AccessMap
-- dummyAccess = const $ amSingle (Slot (-1) Nothing typeVoid) NoAccess
dummyAccess l = error ("Incorrect traversal order at " ++ show l)

wipeNotes :: Core () -> Core AccessMap
wipeNotes = everywhere (mkT poison) . fmap (const mempty)
    where
        poison :: (CoreLine AccessMap, AccessMap)
               -> (CoreLine AccessMap, AccessMap)
        poison (line, _) = (line, dummyAccess line)

markAccess :: Core t -> KaosM (Core (AccessMap))
markAccess cb =
    fmap CB $ everywhereM' (mkM markLine) ls
    where
        ls = 
            let CB l = wipeNotes (fmap (const ()) cb)
            in  l

amSingle :: Slot -> AccessType -> AccessMap
amSingle k v = AM $ M.singleton k v
amFromSA :: SlotAccess -> AccessMap
amFromSA (SA k v) = amSingle k v

markLine :: (CoreLine AccessMap, AccessMap) -> KaosM (CoreLine AccessMap, AccessMap)
markLine (line, _) = do
    accM <- markLine' line
    return (line, accM)

markLine' :: CoreLine AccessMap -> KaosM AccessMap
markLine' (CoreLine tokens) = do
        return . mconcat $ map findAccess tokens
    where findAccess (TokenSlot sa) = amFromSA sa
          findAccess _              = mempty
markLine' (CoreTouch sa) = return $ amFromSA sa


markLine' (CoreAssign s1 s2) = return $
    amSingle s1 WriteAccess `mappend` amSingle s2 ReadAccess

markLine' (CoreConst s1 _) = return $ amSingle s1 WriteAccess

-- XXX: Needs to be sensitive to context
markLine' (CoreCond condition ifTrue ifFalse) =
    do
        condA  <- markLine' (CoreLine condition)
        trueA  <- blockAccess ifTrue
        falseA <- blockAccess ifFalse
        return $ condA `mappend` trueA `mappend` falseA

markLine' (CoreLoop body) = do
    body' <- markAccess body
    let AM bodyAccess = blockAccess' body'
    bodyFutures <- fmap snd $ markBlockFuture' M.empty body'
    let bodyAcc' = M.mapWithKey (merge bodyFutures) bodyAccess
    return $ AM bodyAcc'
    where   
        merge future slot access = merge' (M.lookup slot future) access
        merge' Nothing      ReadAccess    = ReadAccess
        merge' Nothing      _             = WriteAccess
        merge' (Just Read)  ReadAccess    = ReadAccess
        merge' x            y             = trace ("merge' fallback: " ++ show (x, y)) MutateAccess
markLine' (CoreTargReader ts s body) = do
    bodyA <- blockAccess body
    return $ amSingle ts MutateAccess `mappend` amSingle s ReadAccess `mappend` bodyA
markLine' (CoreTargWriter s body) = do
    bodyA <- blockAccess body
    return $ amSingle s WriteAccess `mappend` bodyA

markLine' l@(CoreTypeSwitch _ _ _ _) = error $ "Late typeswitch: " ++ show l
markLine' n@(CoreNote _) = error $ "CoreNote unimplemented" ++ show n

baMergeAM :: AccessMap
          -> M.Map Slot AccessType
          -> M.Map Slot AccessType
-- XXX: can this be merged with Lookahead somehow?
baMergeAM (AM a) b = M.unionWith mergeOne a b
    where
        mergeOne x WriteAccess = x
        mergeOne NoAccess x = x
        mergeOne x NoAccess = x
        mergeOne WriteAccess _ = NoAccess
        mergeOne MutateAccess _ = MutateAccess
        mergeOne ReadAccess x = x

blockAccess' :: Core AccessMap
             -> AccessMap
blockAccess' = AM . foldr baMergeAM M.empty . map snd . unCB

blockAccess :: Core a -> KaosM AccessMap
blockAccess b = fmap blockAccess' $ markAccess b

