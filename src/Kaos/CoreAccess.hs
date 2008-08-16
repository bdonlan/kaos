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
module Kaos.CoreAccess (markAccess, mergeAM, lineAccess) where

import Kaos.Core
import Kaos.CoreTraverse
import Kaos.Slot
import qualified Data.Map as M
import Data.Monoid
import Kaos.CoreFuture
import Debug.Trace
import Kaos.KaosM
import Control.Monad
import Kaos.AST

markAccess :: Core t -> KaosM (Core (AccessMap))
markAccess = bottomUpMark lineAccess

amSingle :: Slot -> AccessType -> AccessMap
amSingle k v = AM $ M.singleton k v
amFromSA :: SlotAccess -> AccessMap
amFromSA (SA k v) = amSingle k v
amEmpty :: AccessMap
amEmpty = AM $ M.empty

lineAccess :: CoreLine AccessMap -> KaosM AccessMap
lineAccess (CoreLine tokens) = do
        return . mconcat $ map findAccess tokens
    where findAccess (TokenSlot sa)         = amFromSA sa
          findAccess (TokenConstSlot s _)   = amSingle s ReadAccess
          findAccess _                      = mempty
lineAccess (CoreTouch sa) = return $ amFromSA sa


lineAccess (CoreAssign s1 s2) = return $
    amSingle s1 WriteAccess `mappend` amSingle s2 ReadAccess

lineAccess (CoreConst s1 _) = return $ amSingle s1 WriteAccess

-- XXX: Needs to be sensitive to context
lineAccess (CoreCond condition ifTrue ifFalse) =
    do
        condA  <- lineAccess (CoreLine condition)
        trueA  <- blockAccess ifTrue
        falseA <- blockAccess ifFalse
        let rv  = condA `mappend` (promoteWrites $ trueA `mappend` falseA)
        return rv
    where
        promoteWrites (AM m) = AM $ M.fromList $ map promote $ M.toList m
            where
                promote (k, WriteAccess) = (k, MutateAccess)
                promote p = p

lineAccess (CoreLoop body) = do
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
    	merge' _            MutateAccess  = MutateAccess
        merge' x            y             = trace ("merge' fallback: " ++ show (x, y)) MutateAccess
lineAccess (CoreTargReader ts s body) = do
    bodyA <- blockAccess body
    return $ amSingle ts MutateAccess `mappend` amSingle s ReadAccess `mappend` bodyA
lineAccess (CoreTargWriter s body) = do
    bodyA <- blockAccess body
    return $ amSingle s WriteAccess `mappend` bodyA

lineAccess (CoreNote _) = return amEmpty
lineAccess (CoreFoldable _ l) = lineAccess l
lineAccess (CoreInlineFlush _) = return amEmpty
lineAccess (CoreInlineAssign _ _ dest replacement) = do
    replMap <- lineAccess (CoreLine replacement)
    return $ amSingle dest WriteAccess `mappend` replMap
lineAccess CoreTargZap = return amEmpty

baMergeAM :: AccessMap
          -> M.Map Slot AccessType
          -> M.Map Slot AccessType
baMergeAM a b = getAM $ a `mappend` AM b
{-
-- XXX: can this be merged with Lookahead somehow?
baMergeAM (AM a) b = M.unionWith mergeOne a b
    where
        mergeOne x WriteAccess = x
        mergeOne NoAccess x = x
        mergeOne x NoAccess = x
        mergeOne WriteAccess _ = NoAccess
        mergeOne MutateAccess _ = MutateAccess
        mergeOne ReadAccess x = x
-}
mergeAM :: AccessMap
        -> AccessMap
        -> AccessMap
mergeAM a (AM b) = AM $ baMergeAM a b

blockAccess' :: Core AccessMap
             -> AccessMap
blockAccess' = AM . foldr baMergeAM M.empty . map snd . unCB

blockAccess :: Core a -> KaosM AccessMap
blockAccess b = fmap blockAccess' $ markAccess b

