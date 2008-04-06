module Kaos.CoreTraverse (bottomUpMark, mapCoreLinesM, mapCoreM) where

import Kaos.Core
import Control.Monad

bottomUpMark :: Monad m
             => (CoreLine b -> m b)
             -> (Core a -> m (Core b))
bottomUpMark f = mapCoreM (\l _ -> do { m <- f l; return (l, m) })

mapCoreLinesM :: Monad m
              => (CoreLine () -> m (CoreLine ()))
              -> (Core a -> m (Core ()))
mapCoreLinesM f = mapCoreM (\l _ -> do { l' <- f l; return (l', ()) })

mapCoreM    :: Monad m
            => (CoreLine b -> a -> m (CoreLine b, b))
            -> (Core a -> m (Core b))
mapCoreM f (CB ls) = liftM CB $ mapM processLine ls
    where
        processLine (l, m) = do
            l' <- recurseLineM f l
            f l' m

recurseLineM :: Monad m
             => (CoreLine b -> a -> m (CoreLine b, b))
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
    liftM2 (CoreCond t) (mapCoreM f b1) (mapCoreM f b2)
recurseLineM f (CoreLoop b) = liftM CoreLoop $ mapCoreM f b
recurseLineM f (CoreTargReader s1 s2 b) =
    liftM (CoreTargReader s1 s2) $ mapCoreM f b
recurseLineM f (CoreTargWriter s b) = liftM (CoreTargWriter s) $ mapCoreM f b
recurseLineM _ CoreTargZap = return CoreTargZap
