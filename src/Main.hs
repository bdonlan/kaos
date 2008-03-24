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

module Main (main) where

import System.IO
import System.Environment
import System.Exit
import Control.Monad
import Text.ParserCombinators.Parsec

import Kaos.Toplevel

import Kaos.Parser
import Kaos.Compile
import Kaos.Prelude

import qualified Data.ByteString.Lazy.Char8 as LBS


--import Debug.Trace

data Settings = Settings {
    helpOnly :: Bool,
    versionOnly :: Bool,
    sourceFiles :: [String],
    outputFile :: String,
    debugFlags :: [String]
    }

instance Show Settings where
    show s = unlines $
        ["helpOnly:    " ++ (show $ helpOnly    s)
        ,"versionOnly: " ++ (show $ versionOnly s)
        ,"sourceFiles: " ++ (show $ sourceFiles s)
        ,"outputFile:  " ++ (show $ outputFile  s)
        ]
        

defaults :: Settings
defaults = Settings {
    helpOnly = False,
    versionOnly = False,
    sourceFiles = [],
    outputFile = "",
    debugFlags = []
    }

parseArgs :: Settings -> [String] -> IO Settings
parseArgs s ("--help":_) = return $ s { helpOnly = True }
parseArgs s ("--version":_) = return $ s { versionOnly = True }
parseArgs _ ((a@('-':'-':_)):_) = fail $ "Unknown argument " ++ a
parseArgs s (('-':t@(_:_)):remain) = parseShortArgs s t remain

parseArgs s (file:t) =
    parseArgs (s { sourceFiles = file:sourceFiles s }) t

parseArgs s [] = return s

setOutputFile :: Settings -> String -> IO Settings
setOutputFile _ "" = fail "Cannot use an empty string as an output file"

setOutputFile s file =
    case outputFile s of
        "" -> return $ s { outputFile = file }
        _  -> fail "Can't set output file twice"

parseShortArgs :: Settings -> String -> [String] -> IO Settings
parseShortArgs _ [] _ = error "impossible"
parseShortArgs s ('h':_) _ = return $ s { helpOnly = True }
parseShortArgs s ('v':_) _ = return $ s { versionOnly = True }

parseShortArgs s ('o':file@(_:_)) remain =
    do  s' <- setOutputFile s file
        parseArgs s' remain

parseShortArgs s ('o':[]) (file:remain) =
    do  s' <- setOutputFile s file
        parseArgs s' remain

parseShortArgs _ ('o':[]) [] = fail "-o requires an argument"

parseShortArgs s ('d':flag@(_:_)) remain =
    parseArgs (s { debugFlags = flag:(debugFlags s) }) remain

parseShortArgs s ('d':[]) (flag:remain) =
    parseArgs (s { debugFlags = flag:(debugFlags s) }) remain

parseShortArgs _ (x:_) _ = fail $ "Unknown short argument -" ++ [x]

versionStr :: String
versionStr = "Kaos v0.0"
helpStr :: String
helpStr = unlines $ versionStr:
    ["Usage: kaos [-o file] [--help] [--version] file [...]"
    ,""
    ,"Arguments:"
    ,"-o file   Write output CAOS to this file"
    ,"--help    Shows this text"
    ,"--version Show program version"
    ]

setDefaultOutputFile :: Settings -> Settings
setDefaultOutputFile s =
    case outputFile s of
        "" -> s { outputFile = "output.cos" }
        _  -> s

main :: IO ()
main = do
    argv <- getArgs
    s <- parseArgs defaults argv
    let s' = setDefaultOutputFile s
    if helpOnly s'
        then putStr helpStr
        else if versionOnly s'
            then putStrLn versionStr
            else doCompile s'

readSourceFile :: String -> IO (String, String)
readSourceFile "-" = do
    s <- hGetContents stdin
    return ("(stdin)", s)
readSourceFile fn = do
    s <- readFile fn
    return (fn, s)

withOutputFile :: String -> (Handle -> IO a) -> IO a
withOutputFile "-" m = m stdout
withOutputFile file m = withFile file WriteMode m

loadPrelude :: Settings -> IO [KaosUnit]
loadPrelude s = do
    if "no-implicit-prelude" `elem` debugFlags s
        then return []
        else parseString "(internal prelude)" preludeStr

doCompile :: Settings -> IO ()
doCompile s = do
    when ((length $ sourceFiles s) == 0) $ fail "No source files"
    prelude <- loadPrelude s
    parses <- mapM parseFile (sourceFiles s)
    let merged = concat (prelude:parses)
    result <- compile (debugFlags s) merged
    case result of 
        Nothing -> exitFailure
        Just str -> withOutputFile (outputFile s) (\h -> LBS.hPut h str)

parseFile :: String -> IO (KaosSource)
parseFile fn = do
    (name, contents) <- readSourceFile fn
    parseString name contents

parseString :: String -> String -> IO KaosSource
parseString name contents = do
    let result = runParser parser () name contents
    case result of
        Right x -> return x
        Left e -> do
            hPutStrLn stderr "Parse error:"
            hPrint stderr e
            exitFailure
