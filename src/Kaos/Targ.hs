module Kaos.Targ (targExpand) where

--import Control.Monad (guard)
import Control.Monad.Cont

import Data.Generics
import qualified Data.Map as M
import Data.Maybe

--import Debug.Trace

import Kaos.AST
import Kaos.Core
import Kaos.Slot
import Kaos.KaosM
import Kaos.CoreAccess
import Kaos.CoreAlias

import qualified Kaos.AliasMap as AM

debugDumpCore :: Show t => String -> Core t -> KaosM (Core t)
debugDumpCore tag core = do
    debugDump tag (dumpCore $ fmap (const ()) core)
    debugDump (tag ++ "-raw") (dumpCore core)
    return core

targExpand :: Core t -> KaosM (Core ())
targExpand d = return (fmap (const ()) d)
           >>= tagTempSlots
           >>= debugDumpCore "dump-targ-marked"
           >>= markAccess
           >>= expandForward
           >>= debugDumpCore "dump-targ-expanded"
           >>= markAccess -- may be out of date
           >>= markAliasing
           >>= mergeAdjacent
           >>= debugDumpCore "dump-targ-merged"
           >>= (return . (fmap (const ())))
           >>= stripTarg
           >>= debugDumpCore "dump-targ-strip"

tagTempSlots :: Core () -> KaosM (Core ())
tagTempSlots = everywhereM (mkM tagLine)
    where
        tagLine :: CoreLine () -> KaosM (CoreLine ())
        tagLine (CoreTargReader _ s b) = do
            ts <- newSlot typeObj
            return $ CoreTargReader ts s b
        tagLine l = return l

usesTarg :: CoreLine t -> Bool
usesTarg core = worker `runCont` id
    where
        worker = callCC $ \ret -> do
            everywhereM (mkM (checkTarg ret)) $ fmap (const ()) core
            return False
        checkTarg :: (Bool -> Cont Bool (CoreLine ()))
                  -> CoreLine ()
                  -> Cont Bool (CoreLine ())
        checkTarg ret (CoreTargReader _ _ _) = ret True
        checkTarg ret (CoreTargWriter _ _) = ret True
        checkTarg _ t = return t

expandForward :: Core AccessMap -> KaosM (Core AccessMap)
expandForward = return . everywhere (mkT expandOne)
    where
        expandOne :: [(CoreLine AccessMap, AccessMap)]
                  -> [(CoreLine AccessMap, AccessMap)]
        -- Special case: When expanding a sequence like this:
        --   .targ > $obj { ... }; .let $var = $obj; .targ < $var { ... } ;
        -- we must manually merge the assignment there. Currently this is
        -- done by merging /backwards/ like so:
        --
        -- .let $var = $obj; .targ < $var { ... }; 
        -- becomes
        -- .targ < $obj { .let $var = $obj; ... };
        --
        -- We also handle the case where:
        -- .let $var = $obj;
        -- .targ < $obj { ... };
        -- similarly
        expandOne (a@(CoreAssign vdest vsrc, am1):(CoreTargReader ts s (CB blk), am2):remain)
            | vdest == s || vsrc == s
            = (CoreTargReader ts vsrc (CB $ a:blk), mergedAM):remain
            where
                mergedAM = am1 `mergeAM` am2
        expandOne orig@((curL, curAM):(nextL, nextAM):remain)
            | not eligible || usesTarg nextL || (isWrite && usesSlot)
            = orig
            | otherwise
            = (newL, curAM `mergeAM` nextAM):remain
            where
                (eligible, isWrite, mySlot, newL) = check curL
                check (CoreTargReader ts s (CB block))
                    =   (True
                        ,False
                        ,s
                        ,CoreTargReader ts s (CB $ block ++ [(nextL, nextAM)])
                        )
                check (CoreTargWriter s (CB block))
                    =   (True
                        ,True
                        ,s
                        ,CoreTargWriter s(CB $ block ++ [(nextL, nextAM)])
                        )
                check _ = (False, undefined, undefined, undefined)
                usesSlot =
                    isJust $ M.lookup mySlot nextAM' >>= guard . (/= NoAccess)
                nextAM' = getAM $ getLineAccess nextAM
        expandOne l = l

mergeAdjacent :: Core AliasTag -> KaosM (Core AliasTag)
mergeAdjacent = return . everywhere (mkT mergeBlock)
    where
        mergeBlock :: CoreBlock AliasTag -> CoreBlock AliasTag
        mergeBlock (CB ls) = CB $ slidingMerge ls
            where
                slidingMerge (a@(lineA, aliasA):b@(lineB, aliasB):r)
                    = case tryMerge lineA lineB aliasA aliasB of
                        Just c  -> slidingMerge (c:r)
                        Nothing -> a:(slidingMerge (b:r))
                slidingMerge l = l

                tryMerge (CoreTargReader ts1 s1 (CB blk1)) (CoreTargReader ts2 _ (CB blk2)) _ rAlias
                    | AM.aliases ts1 ts2 rAlias
                    = Just $ (CoreTargReader ts1 s1 (CB (blk1 ++ blk2)), rAlias)
                tryMerge (CoreTargWriter s1 (CB blk1)) (CoreTargReader _ s2 (CB blk2)) wAlias rAlias
                    | AM.aliases s1 s2 wAlias
                    = Just $ (CoreTargWriter s1 (CB $ blk1 ++ [(targAssign s1, undefined)] ++ blk2), rAlias)
                tryMerge _ _ _ _ = Nothing

stripTarg :: Core () -> KaosM (Core ())
stripTarg = return . everywhere (mkT stripOneTarg)
    where
        stripOneTarg :: [(CoreLine (), ())] -> [(CoreLine (), ())]
        stripOneTarg = map (\l -> (l, ())) . stripOneTarg' . map fst
        stripOneTarg' :: [CoreLine ()] -> [CoreLine ()]
        stripOneTarg' ((CoreTargReader tempslot slot block):remain) =
               [(CoreAssign tempslot slot),(CoreLine [TokenLiteral "targ", TokenSlot (SA tempslot ReadAccess)])]
            ++ (map fst . unCB $ block)
            ++ remain
        stripOneTarg' ((CoreTargWriter slot block):remain) =
               (map fst . unCB $ block)
            ++ [targAssign slot]
            ++ remain
        stripOneTarg' l = l

targAssign :: Slot -> CoreLine a
targAssign slot = CoreLine [TokenLiteral "seta", TokenSlot (SA slot WriteAccess), TokenLiteral "targ"]
