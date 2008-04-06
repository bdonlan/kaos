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
module Kaos.CoreFold (performFolding, stripFolds) where

import Control.Monad.State

import qualified Data.Map as M

import Kaos.AST
import Kaos.Core
import Kaos.CoreTraverse
import Kaos.CoreAccess
import Kaos.KaosM
import Kaos.Slot
import qualified Kaos.AliasMap as AM

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f l = liftM concat $ mapM f l

type Value = ConstValue

data AliasState = AS { asModified :: Bool
                     , asSlotMap  :: AM.GroupMap Slot Value
                     }
                        
initState :: AliasState
initState = AS False AM.empty

getSM :: AliasM (AM.GroupMap Slot Value)
getSM = gets asSlotMap

modifySM :: (AM.GroupMap Slot Value -> AM.GroupMap Slot Value) -> AliasM ()
modifySM f = modify $ \s -> s { asSlotMap = f $ asSlotMap s }

putSM :: AM.GroupMap Slot Value -> AliasM ()
putSM = modifySM . const

type AliasM t = StateT AliasState KaosM t

maxIterations :: Int
maxIterations = 16

performFolding :: Core () -> KaosM (Core ())
performFolding = performFolding' maxIterations

performFolding' :: Int -> Core () -> KaosM (Core ())
performFolding' 0 core = return core
performFolding' remain core = do
    amCore <- markAccess core
    (newcore, AS modified _) <- doFold amCore
    if modified
        then performFolding' (pred remain) newcore
        else return core

doFold :: Core AccessMap -> KaosM (Core (), AliasState)
doFold core = runStateT (foldBlock core) initState

foldBlock :: CoreBlock AccessMap -> AliasM (Core ())
foldBlock (CB ls) = do
    ls' <- concatMapM (uncurry foldLine) ls
    return . CB . map (\l -> (l, ())) $ ls'

foldLine :: CoreLine AccessMap -> AccessMap -> AliasM [CoreLine ()]

foldLine (CoreAssign dest src) _ = do
    modifySM $ AM.aliasTo src dest
    return [CoreAssign dest src]

foldLine (CoreConst dest cval) _ = do
    modifySM $ AM.remove dest
    modifySM $ AM.setGroupVal dest (Just cval)
    return [CoreConst dest cval]

foldLine (CoreCond cond if_ else_) _ = do
    st <- getSM
    if_' <- foldBlock if_
    st1 <- getSM
    putSM st
    else_' <- foldBlock else_
    st2 <- getSM
    putSM $ AM.mergeForUnchanged st st1 st2
    return $ [CoreCond cond if_' else_']

foldLine (CoreLoop body) _ = do
    oldstate <- get
    st <- getSM
    foldBlock body
    st' <- getSM
    put oldstate
    putSM $ AM.mergeForUnchanged st st st'
    body' <- foldBlock body
    st'' <- getSM
    putSM $ AM.mergeForUnchanged st st' st''
    return $ [CoreLoop body']

foldLine (CoreFoldable f base) am = do
    st <- getSM
    -- if this next line gives us more than one line... well, don't fold in a
    -- fold, mmmk?
    [base'] <- foldLine base am -- keep our alias state up to date in any case
    case f (\s -> AM.getGroupVal s st) of
        Nothing -> do
            return $ [CoreFoldable f base']
        Just ls -> do
            modify $ \as -> as { asModified = True }
            return ls

foldLine line am = do
    let am' = getAM am
    mapM_ (modifySM . AM.remove . fst) . filter ((> ReadAccess).snd) . M.toList $ am'
    return [fmap (const ()) line]

stripFolds :: Core () -> KaosM (Core ())
stripFolds = mapCoreLinesM (return . stripLine)
    where
        stripLine :: CoreLine () -> CoreLine ()
        stripLine (CoreFoldable _ l) = l
        stripLine l = l
