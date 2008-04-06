module Kaos.CoreTraverse (bottomUpMark) where

import Kaos.Core
import Kaos.KaosM
import Control.Monad

bottomUpMark :: (CoreLine b -> KaosM b)
            -> (Core a -> KaosM (Core b))
bottomUpMark f (CB ls) = liftM CB $ mapM processLine ls
    where
        processLine (l, _) = do
            l' <- bottomUpRecurse f l
            m  <- f l'
            return (l', m)

bottomUpRecurse :: (CoreLine b -> KaosM b)
                -> (CoreLine a -> KaosM (CoreLine b))
bottomUpRecurse _ l@(CoreLine _) = return $ fmap undefined l
bottomUpRecurse _ l@(CoreAssign _ _) = return $ fmap undefined l
bottomUpRecurse _ l@(CoreInlineAssign{}) = return $ fmap undefined l
bottomUpRecurse _ l@(CoreInlineFlush _) = return $ fmap undefined l
bottomUpRecurse _ l@(CoreConst _ _) = return $ fmap undefined l
bottomUpRecurse _ l@(CoreNote _) = return $ fmap undefined l
bottomUpRecurse _ l@(CoreFoldable _ _) = return $ fmap undefined l
bottomUpRecurse _ l@(CoreTouch _) = return $ fmap undefined l
bottomUpRecurse f (CoreCond t b1 b2) =
    liftM2 (CoreCond t) (bottomUpMark f b1) (bottomUpMark f b2)
bottomUpRecurse f (CoreLoop b) = liftM CoreLoop $ bottomUpMark f b
bottomUpRecurse f (CoreTargReader s1 s2 b) =
    liftM (CoreTargReader s1 s2) $ bottomUpMark f b
bottomUpRecurse f (CoreTargWriter s b) = liftM (CoreTargWriter s) $ bottomUpMark f b
bottomUpRecurse _ CoreTargZap = return CoreTargZap
