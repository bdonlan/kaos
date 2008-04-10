#!/usr/bin/env runhaskell

> import Distribution.Simple
> import System.FilePath
> import System.Directory
> import Data.List
> import Control.Monad
> import System.Time (ClockTime)

> heads = unfoldr step . Just
>    where
>       step Nothing        = Nothing
>       step (Just [])      = Just ([], Nothing)
>       step (Just (x:xs))  = Just (x, Just xs)

> preludeSrcDir	= "prelude"
> preludePath = "gen/Kaos/Prelude.hs"
> preludeDir  = fst $ splitFileName preludePath
> preludeDirs = reverse $ map combine $ heads $ splitPath preludeDir

> hooks = defaultUserHooks { preBuild = checkPrelude, cleanHook = cleanPreludeHook }
> main = defaultMainWithHooks hooks
 
> cleanPreludeHook pd lbi uh cf = do
>   cleanPrelude
>   cleanHook defaultUserHooks pd lbi uh cf

> checkPreludeDirs = createDirectoryIfMissing True preludeDir
> cleanPrelude = do
>	checkPreludeDirs
>	removeDirectoryRecursive "gen"

> loadPreludeSource :: IO (String, ClockTime)
> loadPreludeSource = do
>   entries <- liftM sort $ getDirectoryContents preludeSrcDir
>   l <- mapM (scanEnt . ((preludeSrcDir ++ "/")++)) entries
>   let (strs, modTimes) = unzip $ concat l
>   return $ (concat strs, maximum modTimes)
>   where
>     scanEnt e
>       | not (".k" `isSuffixOf` e)
>       = return []
>		| otherwise
>		= do
>			contents <- readFile e
>			modTime  <- getModificationTime e
>			return [(contents, modTime)]

> checkPrelude args buildflags = do
>   checkPreludeDirs
>   (preludeSrc, preludeModTime) <- loadPreludeSource
>   pe <- doesFileExist preludePath
>   when (not pe) $ genPrelude preludeSrc
>   lastGen <- getModificationTime preludePath
>   when (preludeModTime >= lastGen) $ genPrelude preludeSrc
>   preBuild defaultUserHooks args buildflags

> genPrelude src = do
>   writeFile preludePath $ unlines [
>       "module Kaos.Prelude (preludeStr) where",
>       "preludeStr :: String",
>       "preludeStr = " ++ show src
>       ]
