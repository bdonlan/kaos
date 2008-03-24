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
module Kaos.CoreInline (inlineValues, inlineAnalysis, inlineFallback) where

import Kaos.AST
import Kaos.Core
import Kaos.CoreAccess
import Kaos.CoreFuture
import Kaos.KaosM
import Kaos.Slot
import Kaos.SeqT

import Data.Generics
import Data.Typeable
import Data.Maybe
import Data.List
import qualified Data.Map as M

import Control.Monad.State

newtype InlineID = IID Int
    deriving (Eq, Ord, Show, Data, Typeable, Bounded, Enum)

data InlineEntry = IE   { ieString  :: [CoreToken]
                        , ieType    :: CAOSType
                        , ieLevel   :: Int
                        , ieTargUser:: Bool
                        }
    deriving (Show, Data, Typeable)

data InlineState = IS   { ieGroups      :: M.Map InlineID InlineEntry
                        , ieSlotState   :: M.Map Slot InlineID
                        , ieGroupLevel  :: M.Map Int [InlineID]
                        }

initState :: InlineState
initState = silenceWarnings `seq` IS M.empty M.empty M.empty
    where
        silenceWarnings = ieType `seq` ieLevel

type InlineM a = StateT InlineState (SeqT InlineID (StateT Bool KaosM)) a

inlineAnalysis :: Core () -> KaosM (Core ())
inlineAnalysis = return . markTargUsage

markTargUsage :: Core () -> Core ()
markTargUsage = scan . markUser False
    where
        scan :: Data b => b -> b
        scan t
            | isNothing line
            = gmapT scan t
            | otherwise
            = extT id scanLine t
            where
                line :: Maybe (CoreLine ())
                line = cast t
        scanLine :: CoreLine () -> CoreLine ()
        scanLine (CoreTargReader s1 s2 block) = CoreTargReader s1 s2 (markUser True block)
        scanLine (CoreTargWriter s block) = CoreTargWriter s (markUser True block)
        scanLine l = l

        markUser state = everywhere (mkT (markLine state))

        markLine :: Bool -> CoreLine () -> CoreLine ()
        markLine st l@(CoreInlineAssign _ _ _ _) = l { ciaTargUser = st }
        markLine _  l = l

madeProgress :: InlineM ()
madeProgress = lift . lift $ put True

inlineValues :: Core t -> KaosM (Core ())
inlineValues  = inlineValues'

inlineValues' :: Core t -> KaosM (Core ())
inlineValues' = rep 10 pipeline . scrub
    where
        enterInlineM :: InlineM a -> StateT Bool KaosM a
        enterInlineM = runSeqT (IID 0) . flip evalStateT initState
        scrub = fmap (const ())
        rep 0 _ r = return r
        rep n m r = do
            (r', s) <- runStateT (m r) False
            if s
                then rep (pred n) m r'
                else return r'
        pipeline :: Core () -> StateT Bool KaosM (Core ())
        pipeline d = return d
                >>=  lift . markAccess
                >>=  lift . markFuture
                >>=  enterInlineM . inlineCore 
                >>=  return . scrub

inlineCore :: Core FutureS -> InlineM (Core FutureS)
inlineCore (CB xs) = fmap (CB . map (\x -> (x, undefined))) $ mapM doLine xs
    where
        doLine (line, info) = do
            line' <- earlyInlineLine line
            mapM (uncurry touchSlot) . M.toList . getAM $ getLineAccess info
            inlineLine line'

touchSlot :: Slot -> AccessType -> InlineM ()
touchSlot slot acc
    | acc > ReadAccess
    = zap slot
    | otherwise
    = return ()

inlineToken :: CoreToken -> InlineM ([CoreToken])
inlineToken l@(TokenSlot (SA slot ReadAccess)) = do
    entry' <- getSlot slot
    case entry' of
        Nothing -> return [l]
        Just entry -> do
            madeProgress
            return $ ieString entry
inlineToken l = return [l]


getSlot        :: Slot -> InlineM (Maybe InlineEntry)
getSlot slot = do
    iid' <- getSlotIID slot
    case iid' of
        Nothing -> return Nothing
        Just iid -> do
            entry <- gets (M.lookup iid . ieGroups)
            when (isNothing entry) $ zap slot
            return entry

getSlotIID     :: Slot -> InlineM (Maybe InlineID)
getSlotIID slot = gets (M.lookup slot . ieSlotState)

putSlot     :: Slot -> Maybe InlineID -> InlineM ()
putSlot slot Nothing = zap slot
putSlot slot (Just iid) =
    modify $ \s -> s { ieSlotState = M.insert slot iid $ ieSlotState s }
zap         :: Slot -> InlineM ()
zap slot = modify $ \s -> s { ieSlotState = M.delete slot $ ieSlotState s }

earlyInlineLine :: CoreLine FutureS -> InlineM (CoreLine FutureS)

earlyInlineLine (CoreCond tokens trueBranch falseBranch) = do
    tokens' <- fmap concat $ mapM inlineToken tokens
    return $ CoreCond tokens' trueBranch falseBranch
earlyInlineLine l = return l

inlineLine :: CoreLine FutureS -> InlineM (CoreLine FutureS)
inlineLine (CoreLine l) = do
    l' <- fmap concat $ mapM inlineToken l
    return $ CoreLine l'
inlineLine l@(CoreAssign dest src) = do
    putSlot dest =<< getSlotIID src
    return l
inlineLine l@(CoreInlineAssign level targuser slot replacement) = do
    let entry = IE replacement (slotType slot) level targuser
    gid <- lift getNext
    s <- get
    put $ s { ieGroups      = M.insert gid entry (ieGroups s)
            , ieGroupLevel  = M.insertWith (++) level [gid] (ieGroupLevel s)
            , ieSlotState   = M.insert slot gid $ ieSlotState s
            }
    return l
inlineLine l@(CoreInlineFlush level) = do
    s <- get
    let (noflush, levelTarget, flushme') = M.splitLookup level (ieGroupLevel s)
    -- split and splitLookup don't put the actual key in the flushme' result,
    -- so add it back. Also pull out the InlineIDs
    let flushme = (fromMaybe [] levelTarget) ++ concat (M.elems flushme')
    let groups' = foldl' (flip M.delete) (ieGroups s) flushme
    -- slotState will be reset by getSlot operations, so we don't worry about it
    put $ s { ieGroups = groups', ieGroupLevel = noflush }
    return l
inlineLine l@(CoreConst _ _) = return l
inlineLine l@(CoreNote _) = return l
inlineLine l@(CoreTouch _) = return l
inlineLine (CoreCond tok b1 b2) = do -- tok handled in earlyInlineLine
    s <- get -- since we've used the accessmap to zap slots as needed, this should be
             -- safe and conservative
    b1' <- inlineCore b1
    put s
    b2' <- inlineCore b2
    put s
    return $ CoreCond tok b1' b2'
inlineLine (CoreLoop body) = do
    s <- get
    body' <- inlineCore body
    put s
    return $ CoreLoop body'
inlineLine (CoreFoldable _ _) = fail "late foldable in inliner"
inlineLine (CoreTargReader tempslot srcslot block) = do
    block' <- inlineCore block
    modify $ \s -> s { ieGroups = M.filter (not . ieTargUser) (ieGroups s) }
    return $ CoreTargReader tempslot srcslot block'
inlineLine (CoreTargWriter destslot block) = do
    block' <- inlineCore block
    modify $ \s -> s { ieGroups = M.filter (not . ieTargUser) (ieGroups s) }
    return $ CoreTargWriter destslot block'
inlineLine CoreTargZap = do
    modify $ \s -> s { ieGroups = M.filter (not . ieTargUser) (ieGroups s) }
    return CoreTargZap

inlineFallback :: CoreLine a -> CoreLine a
inlineFallback l@(CoreInlineAssign _ _ ds repl) =
    CoreLine $ [TokenLiteral setverb, TokenSlot (SA ds WriteAccess)] ++ repl
    where
        t = slotType ds
        setverb
            | t == typeNum
            = "SETV"
            | t == typeStr
            = "SETS"
            | t == typeObj
            = "SETA"
            | otherwise
            = error $ "bad type in inline fallback " ++ show (fmap (const ()) l)
inlineFallback l = error $ "Internal error: Use of inlineFallback on bad line " ++ show (fmap (const ()) l)
