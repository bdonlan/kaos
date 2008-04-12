module Kaos.CoreTraverse (
    bottomUpMark,
    mapCoreLinesM,
    mapCoreM,
    editCoreCtxM,
    editLinesCtxM,
    editLinesCtx
    ) where

import Kaos.Core
--import Control.Monad
import Control.Monad.Identity

-- |Mark every line in the core, starting from the leaves and working up.
bottomUpMark :: Monad m
             => (CoreLine b -> m b)
             -> (Core a -> m (Core b))
bottomUpMark f = mapCoreM (\l _ -> do { m <- f l; return (l, m) })

-- |Edit every line in the core, starting from the leaves and working up
mapCoreLinesM :: Monad m
              => (CoreLine () -> m (CoreLine ()))
              -> (Core a -> m (Core ()))
mapCoreLinesM f = mapCoreM (\l _ -> do { l' <- f l; return (l', ()) })

-- |Edit both the content and marking on every line in the core, starting from
-- the leaves.
mapCoreM    :: forall a b m. Monad m
            => (CoreLine b -> a -> m (CoreLine b, b))
            -> (Core a -> m (Core b))
mapCoreM f = editCoreCtxM id m'
    where
        m' e mark es = do
            (e', mark') <- f e mark
            return ( [(e', mark')], es )

-- |Transform the given core line by applying the given function to any
--  deep blocks.
recurseLineM :: Monad m
             => (Core a -> m (Core b))
             -> (CoreLine a -> m (CoreLine b))
recurseLineM _ l@(CoreLine _) = return $ fmap undefined l
recurseLineM _ l@(CoreAssign _ _) = return $ fmap undefined l
recurseLineM _ l@(CoreInlineAssign{}) = return $ fmap undefined l
recurseLineM _ l@(CoreInlineFlush _) = return $ fmap undefined l
recurseLineM _ l@(CoreConst _ _) = return $ fmap undefined l
recurseLineM _ l@(CoreNote _) = return $ fmap undefined l
recurseLineM _ l@(CoreFoldable _ _) = return $ fmap undefined l
recurseLineM _ l@(CoreTouch _) = return $ fmap undefined l
recurseLineM f (CoreCond t b1 b2) =
    liftM2 (CoreCond t) (f b1) (f b2)
recurseLineM f (CoreLoop b) = liftM CoreLoop $ f b
recurseLineM f (CoreTargReader s1 s2 b) =
    liftM (CoreTargReader s1 s2) $ f b
recurseLineM f (CoreTargWriter s b) = liftM (CoreTargWriter s) $ f b
recurseLineM _ CoreTargZap = return CoreTargZap


-- |Apply a transformation to the Core with context.
--  The transformation function will be provided with a list of later
--  elements in the list, and may arbitrarily modify this before it proceeds
--  to the next element.
--
editCoreCtxM    :: forall a b m. Monad m
                => (forall t. [t] -> [t])   -- ^ A list transformation to
                                            --   apply before and after each
                                            --   scan
                -> (   CoreLine b
                    -> a
                    -> [(CoreLine b, a)]
                    -> m ( [(CoreLine b, b)], [(CoreLine b, a)] )
                   )
                            -- ^ The actual transform to apply. Should return
                            -- a pair of (lines to commit, lines still to be
                            -- processed.
                -> Core a   -- ^ The Core to transform
                -> m (Core b)

editCoreCtxM lf m (CB cl) = liftM (CB . lf) . iter =<< pre (lf cl)
    where
        pre     = mapM recurse
        recurse (li, oldmark) = do
            li' <- recurseLineM (editCoreCtxM lf m) li
            return (li', oldmark)
        iter [] = return []
        iter ((l, mark):ls) = do
            (l', pending) <- m l mark ls
            suffix <- iter pending
            return $ l' ++ suffix

editLinesCtxM :: forall a m. Monad m
              => (forall t. [t] -> [t])
              -> (CoreLine () -> [CoreLine ()] -> m ([CoreLine ()], [CoreLine ()]))
              -> Core a
              -> m (Core ())
editLinesCtxM lf m = editCoreCtxM lf m' . fmap (const ())
    where
        m' li () ls = do
            (commit, pending) <- m li (map fst ls)
            return $ (map mark commit, map mark pending)
        mark x = (x, ())

editLinesCtx :: forall a.
                (forall t. [t] -> [t])
             -> (CoreLine () -> [CoreLine ()] -> ([CoreLine ()], [CoreLine ()]))
             -> Core a
             -> Core ()
editLinesCtx lf f = runIdentity . editLinesCtxM lf (\a b -> return $ f a b)
