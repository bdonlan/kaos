module Kaos.Targ (targExpand) where

--import Control.Monad (guard)
import Control.Monad.Cont

import Data.Generics
import qualified Data.Map as M
import Data.Maybe

import Kaos.AST
import Kaos.Core
import Kaos.Slot
import Kaos.KaosM
import Kaos.CoreAccess
import Kaos.CoreFuture
import Kaos.CoreStorage

{- Semantics of targ blocks:
  
   A CoreTargReader block indicates that the value of targ is to be read
   into the given slot. The prior value of targ is discarded.

   After the CoreTargReader block, the Slot in question will be aliased
   to targ; you may use the slot's value within the block. No assumptions
   about the value of targ prior to entering the block may be made.

   A CoreTargWriter block sets targ to the given slot's value on entry.
   You may modify the slot within the block; do not change targ manually.

   Targ merging and lifting:

   As a preprocessing step, the temporary slot in each CoreTargReader is
   replaced (without examining it strictly) with a new slot.

   (tagTempSlots)

   We then attach storage information.

   We then traverse the Core tree, annotated with storage information,
   in a bottom-up manner, performing the following transformations at each
   CoreBlock:
   (globalTransform)

   - First, all CoreTarg* blocks are extended forward, consuming all
     (non-targ-modifying) lines ahead of them (including entire blocks). Note
     that no CoreTargReader will be expanded back before a write operation
     on their Slot, and no CoreTargWriter will be expanded forward after
     any operation on their slot whatsoever.
     (expandForward)
   - Then, they are extended backwards in a similar manner
     (expandBack)
   - Adjacent blocks are combined in the following manner:
     - Two CoreTargReader blocks may be merged if their slots are mapped to the
       same virtual register at their start.
       (mergeReader)
     - A CoreTargWriter may be merged with a subsequent CoreTargReader
       if their slots are mapped to the same virtual register at the end of
       the writer, and at the beginning of the reader.
       (mergeWriter)
     - No other merges are possible.
   - TODO: Lift CoreTargReader out of loops and if-blocks. Requires duplicate
     code detection of some sort - we'll always have some sort of setup to load
     the slot. Or at least some peephole optimization.
   
   Once we have traversed the entire tree in this manner, we replace
   CoreTargReader and CoreTargWriter with direct commands to load targ etc.
   (stripTarg)
-}

debugDumpCore :: String -> Core t -> KaosM (Core t)
debugDumpCore tag core = do
    debugDump tag (dumpCore $ fmap (const ()) core)
    return core

targExpand :: Core t -> KaosM (Core ())
targExpand d = return (fmap (const ()) d)
           >>= tagTempSlots
           >>= synthesizeAccesses
           >>= debugDumpCore "dump-targ-marked"
           >>= markAccess
           >>= expandForward
           >>= debugDumpCore "dump-targ-expanded"
           >>= markAccess -- may be out of date
           >>= markFuture
           >>= markStorage
           >>= mergeReaders
           >>= mergeWriters
           >>= debugDumpCore "dump-targ-merged"
           >>= (return . (fmap (const ())))
           >>= stripAccesses
           >>= debugDumpCore "dump-targ-strip1"
           >>= stripTarg
           >>= debugDumpCore "dump-targ-strip2"

tagTempSlots :: Core () -> KaosM (Core ())
tagTempSlots = everywhereM (mkM tagLine)
    where
        tagLine :: CoreLine () -> KaosM (CoreLine ())
        tagLine (CoreTargReader _ s b) = do
            ts <- newSlot
            let ts' = ts { slotType = typeObj }
            return $ CoreTargReader ts' s b
        tagLine l = return l

synthesizeAccesses :: Core () -> KaosM (Core ())
synthesizeAccesses = return . everywhere (mkT localAccesses)
    where
        localAccesses :: CoreBlock () -> CoreBlock ()
        localAccesses (CB ls) =
            CB (ls ++ map (\x -> (x, ())) accesses)
            where
                accesses =
                    concatMap (\slot -> [CoreNote (PrivateNote "targ access")
                                        ,CoreTouch (SA slot ReadAccess)
                                        ])
                              tempSlots
                tempSlots = concatMap getTempSlots ls
                getTempSlots ((CoreTargReader ts _ _), ()) = [ts]
                getTempSlots _ = []

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

mergeReaders :: Core StorageS -> KaosM (Core StorageS)
mergeReaders = return

mergeWriters :: Core StorageS -> KaosM (Core StorageS)
mergeWriters = return

stripAccesses :: Core () -> KaosM (Core ())
stripAccesses = return . everywhere (mkT stripOneAccess)
    where
        stripOneAccess :: [(CoreLine (), ())] -> [(CoreLine (), ())]
        stripOneAccess ( (CoreNote (PrivateNote "targ access"), ())
                        :(CoreTouch _, ())
                        :remain
                       )
            = remain
        stripOneAccess x = x

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
            ++ [CoreLine [TokenLiteral "seta", TokenSlot (SA slot WriteAccess), TokenLiteral "targ"]]
            ++ remain
        stripOneTarg' l = l
