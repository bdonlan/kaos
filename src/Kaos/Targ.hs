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

module Kaos.Targ (targExpand, stripTarg) where

--import Control.Monad (guard)
import Control.Monad.Cont

import Data.Generics
import qualified Data.Map as M
import Data.Maybe

import Kaos.AST
import Kaos.Core
import Kaos.CoreTraverse
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
           >>= expandBackward
           >>= debugDumpCore "dump-targ-expanded"
           >>= injectAssignments
           >>= markAccess -- may be out of date
           >>= markAliasing
           >>= mergeAdjacent
           >>= debugDumpCore "dump-targ-merged"
           >>= (return . (fmap (const ())))
           >>= expandForward
           >>= debugDumpCore "dump-targ-final"

tagTempSlots :: Core () -> KaosM (Core ())
tagTempSlots = mapCoreLinesM tagLine
    where
        tagLine :: CoreLine () -> KaosM (CoreLine ())
        tagLine (CoreTargReader _ s b) = do
            ts <- newSlot typeObj
            return $ CoreTargReader ts s b
        tagLine l = return l

usesTarg :: CoreLine a -> Bool
usesTarg line = flip runCont id $ callCC $ \cc -> (mapCoreLinesM (checkLine cc) (CB [(line, undefined)]) >> return False)
    where
        checkLine :: (Bool -> Cont Bool (CoreLine ())) -> CoreLine () -> Cont Bool (CoreLine ())
        checkLine cc (CoreTargReader _ _ _) = cc True
        checkLine cc (CoreTargWriter _ _) = cc True
        checkLine _ l = return l

injectAssignments :: Core a -> KaosM (Core ())
injectAssignments = mapCoreLinesM injectOne
    where
        injectOne :: CoreLine () -> KaosM (CoreLine ())
        injectOne (CoreTargWriter slot (CB ls)) = return $
            CoreTargWriter slot (CB $ ls ++ [(targAssign slot, ())])
        injectOne l = return l

expandForward  :: Core () -> KaosM (Core ())
expandForward = return . editLinesCtx id expandOne
    where
        expandOne :: CoreLine ()
                  -> [CoreLine ()]
                  -> ([CoreLine ()], [CoreLine()])
        -- by now, reads and writes have been merged,
        -- and crucially each write has its assignment statement added
        -- we can now expand forward to swallow up targ-neutral lines
        expandOne p [] = ([p], [])
        expandOne p remain@(line:_)
            | usesTarg line
            = ([p], remain)
        expandOne (CoreTargReader ts s (CB blk)) (lp:ls)
            = ([], (CoreTargReader ts s (CB $ blk ++ [(lp, ())])):ls)
        expandOne (CoreTargWriter s (CB blk)) (lp:ls)
            = ([], (CoreTargWriter s (CB $ blk ++ [(lp, ())])):ls)
        expandOne l ls = ([l], ls)

expandBackward :: Core AccessMap -> KaosM (Core AccessMap)
expandBackward = return . everywhere (mkT expandOne)
    where
        expandOne :: [(CoreLine AccessMap, AccessMap)]
                  -> [(CoreLine AccessMap, AccessMap)]
        -- When an assignment appears before a targread, follow the rename
        expandOne (a@(CoreAssign vdest vsrc, am1):(CoreTargReader ts s (CB blk), am2):remain)
            | vdest == s || vsrc == s
            = (CoreTargReader ts vsrc (CB $ a:blk), mergedAM):remain
            where
                mergedAM = am1 `mergeAM` am2       
        -- Do not expand across other targ blocks; this is a job for later phases
        expandOne xs@((prev, _):_)
            | usesTarg prev
            = xs
        -- Read swallows anything that doesn't write to its variable
        expandOne (target@(_, targetAM):(CoreTargReader ts s (CB blk), am2):xs)
            | not (targetAM `writesSlot` ts || targetAM `writesSlot` s)
            = (CoreTargReader ts s (CB $ target:blk), am2 `mergeAM` targetAM):xs
        -- Write swallows anything at all
        expandOne (target@(_, targetAM):(CoreTargWriter s (CB blk), am2):xs)
            = (CoreTargWriter s (CB $ target:blk), am2 `mergeAM` targetAM):xs
        -- Leave anything else alone
        expandOne xs = xs

writesSlot :: AccessMap -> Slot -> Bool
(AM am) `writesSlot` slot = (fromMaybe NoAccess $ M.lookup slot am) > ReadAccess

mergeAdjacent :: Core AliasTag -> KaosM (Core AliasTag)
mergeAdjacent = editCoreCtxM id step
    where
        step :: CoreLine AliasTag
             -> AliasTag
             -> [(CoreLine AliasTag, AliasTag)]
             -> KaosM ([(CoreLine AliasTag, AliasTag)], [(CoreLine AliasTag, AliasTag)])
        step lineA aliasA [] = return ([ (lineA, aliasA )], [])
        step lineA aliasA (p@(lineB, aliasB):ls) =
            case tryMerge lineA lineB aliasA aliasB of
                Just c -> return ( [], c:ls )
                Nothing -> return ([ (lineA, aliasA) ], p:ls)
        tryMerge (CoreTargReader ts1 s1 (CB blk1)) (CoreTargReader ts2 _ (CB blk2)) _ rAlias
            | AM.aliases ts1 ts2 rAlias
            = Just $ (CoreTargReader ts1 s1 (CB (blk1 ++ blk2)), rAlias)
        tryMerge (CoreTargWriter s1 (CB blk1)) (CoreTargReader _ s2 (CB blk2)) wAlias rAlias
            | AM.aliases s1 s2 wAlias
            = Just $ (CoreTargWriter s1 (CB $ blk1 ++ blk2), rAlias)
        tryMerge _ _ _ _ = Nothing

stripTarg :: Core a -> KaosM (Core ())
stripTarg = editLinesCtxM id stripOneTarg
    where
        stripOneTarg  l ls = return (stripOneTarg' l, ls)
        stripOneTarg' (CoreTargReader tempslot slot block) =
               [(CoreAssign tempslot slot),(CoreLine [TokenLiteral "targ", TokenSlot (SA tempslot ReadAccess)])]
            ++ (map fst . unCB $ block)
            ++ [CoreTargZap]
        stripOneTarg' (CoreTargWriter _ block) =
               (map fst . unCB $ block)
            ++ [CoreTargZap]
        stripOneTarg' l = [l]

targAssign :: Slot -> CoreLine a
targAssign slot = CoreInlineAssign maxBound True slot [TokenLiteral "targ"]
