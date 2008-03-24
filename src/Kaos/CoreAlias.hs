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
module Kaos.CoreAlias (markAliasing, AliasTag) where

import Control.Monad.State

import qualified Data.Map as M

import qualified Kaos.AliasMap as AM
import Kaos.Core
import Kaos.Slot
import Kaos.KaosM

type AliasMap t = AM.AliasMap t
type AliasTag = AliasMap Slot

type AliasM t = StateT AliasTag KaosM t

markAliasing :: Core AccessMap -> KaosM (Core AliasTag)
markAliasing core = evalStateT (markBlock core) AM.empty

intersectTag :: AliasTag
             -> AliasTag
             -> AliasTag
             -> AliasTag
intersectTag = AM.mergeForUnchanged

markBlock :: CoreBlock AccessMap -> AliasM (CoreBlock AliasTag)
markBlock (CB ls) = liftM CB $ mapM (uncurry markLine) ls

markLine :: CoreLine AccessMap -> AccessMap -> AliasM (CoreLine AliasTag, AliasTag)

markLine (CoreCond tok b1 b2) _ = do
    common <- get
    b1' <- markBlock b1
    b1s <- get
    put common
    b2' <- markBlock b2
    modify $ intersectTag common b1s
    st <- get
    return (CoreCond tok b1' b2', st)

markLine (CoreTargReader ts slot body) _ = do
    modify $ AM.aliasTo slot ts
    body' <- markBlock body
    st <- get
    return (CoreTargReader ts slot body', st)

markLine (CoreTargWriter slot body) _ = do
    body' <- markBlock body
    modify $ AM.remove slot
    st <- get
    return (CoreTargWriter slot body', st)

markLine (CoreLoop body) _ = do
    common <- get
    markBlock body
    modify $ intersectTag common common
    body' <- markBlock body
    modify $ intersectTag common common
    st <- get
    return (CoreLoop body', st)

markLine l@(CoreAssign dest src) _ = do
    modify $ AM.aliasTo src dest
    st <- get
    return (coerce l, st)

markLine l am = do
    mapM_ (uncurry purgeWrite) $ M.toList (getAM am)
    st <- get
    return (coerce l, st)
    where
        purgeWrite _ NoAccess = return ()
        purgeWrite _ ReadAccess = return ()
        purgeWrite slot _ = modify $ AM.remove slot

coerce :: CoreLine a -> CoreLine b
coerce = fmap (error "improper use of CoreAlias.coerce")
