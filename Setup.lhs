#!/usr/bin/env runhaskell

> import Distribution.Simple
> import System.FilePath
> import System.Directory
> import Data.List
> import Control.Monad

> heads = unfoldr step . Just
>    where
>       step Nothing        = Nothing
>       step (Just [])      = Just ([], Nothing)
>       step (Just (x:xs))  = Just (x, Just xs)

> preludeSrc  = "src/prelude.k"
> preludePath = "gen/Kaos/Prelude.hs"
> preludeDir  = fst $ splitFileName preludePath
> preludeDirs = reverse $ map combine $ heads $ splitPath preludeDir

> hooks = defaultUserHooks { preBuild = checkPrelude, cleanHook = cleanPreludeHook }
> main = defaultMainWithHooks hooks
 
> cleanPreludeHook pd lbi uh cf = do
>   cleanPrelude
>   cleanHook defaultUserHooks pd lbi uh cf

> checkPreludeDirs = createDirectoryIfMissing True preludeDir
> cleanPrelude = removeDirectoryRecursive "gen"

> checkPrelude args buildflags = do
>   checkPreludeDirs
>   se <- doesFileExist preludeSrc
>   when (not se) $ error ("Prelude file " ++ preludeSrc ++ " does not exist.")
>   pe <- doesFileExist preludePath
>   when (not pe) $ genPrelude
>   smt <- getModificationTime preludeSrc
>   pmt <- getModificationTime preludePath
>   when (smt >= pmt) $ genPrelude
>   preBuild defaultUserHooks args buildflags

> genPrelude = do
>   src <- readFile preludeSrc
>   writeFile preludePath $ unlines [
>       "module Kaos.Prelude (preludeStr) where",
>       "preludeStr :: String",
>       "preludeStr = " ++ show src
>       ]
