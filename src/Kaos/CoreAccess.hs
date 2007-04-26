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

dummyAccess :: a -> AccessMap
-- dummyAccess = const $ amSingle (Slot (-1) Nothing typeVoid) NoAccess
dummyAccess = const $ error "Incorrect traversal order"

markAccess :: Core t -> KaosM (Core (AccessMap))
markAccess cb =
    fmap CB $ everywhereM' (mkM markLine) lines
    where
        lines = 
            let CB l = fmap dummyAccess cb
            in  l

amFromList :: [SlotAccess] -> AccessMap
amFromList  = mconcat
            . map AM
            . map (\sa@(SA k v) -> M.singleton k v)

amSingle k v = AM $ M.singleton k v
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
        trueA  <- fmap blockAccess $ markAccess ifTrue
        falseA <- fmap blockAccess $ markAccess ifFalse
        return $ condA `mappend` trueA `mappend` falseA

markLine' (CoreLoop body) = do
    AM bodyAccess <- fmap blockAccess $ markAccess body
    bodyFutures <- fmap snd $ markBlockFuture' M.empty body
    let bodyAcc' = M.mapWithKey (merge bodyFutures) bodyAccess
    return $ AM bodyAcc'
    where   
        merge future slot access = merge' (M.lookup slot future) access
        merge' Nothing      ReadAccess  = ReadAccess
        merge' Nothing      _           = WriteAccess
        merge' (Just Read)  ReadAccess  = ReadAccess
        merge' x            y           = trace ("merge' fallback: " ++ show (x, y)) MutateAccess
        merge' _            _           = MutateAccess

markLine' x = error $ "Don't know how to markLine' on " ++ show x

blockAccess (CB b) = mconcat . map snd $ b

