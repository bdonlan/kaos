module Kaos.Dump (dumpMap) where

import qualified Data.Map as M

dumpMap :: (Show k, Show v, Ord k) => M.Map k v -> String
dumpMap = unlines . map pairToLine . M.toList
    where pairToLine (k, v) = (show k) ++ " => " ++ (show v)
