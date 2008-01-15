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
