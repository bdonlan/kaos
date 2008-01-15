{-
    Kaos - Kaos compiler
    Copyright (C) 2005  Bryan Donlan

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Main (main) where

import System.IO
import System.Environment
import System.Exit
import Control.Monad
import Text.ParserCombinators.Parsec
import Kaos.Typecheck

import Kaos.AST
import Kaos.ASTTransforms
import Kaos.Core
import Kaos.KaosM

import Kaos.Parser
import Kaos.Rename
import Kaos.ASTToCore
import Kaos.CoreToVirt
import Kaos.CoreAccess
import Kaos.CoreFuture
import Kaos.CoreStorage
import Kaos.CoreFold
import Kaos.Targ
import Kaos.RegAlloc
import Kaos.Emit


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
versionStr = "HaKaos v0.0"
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

openSourceFile :: String -> IO (String, Handle)
openSourceFile "-" = return ("(stdin)", stdin)
openSourceFile file = do
    h <- openFile file ReadMode
    return (file, h)

dumpFlagged :: String -> (t -> String) -> t -> KaosM t
dumpFlagged flag f v = do
    debugDump flag (f v)
    return v

coreCompile :: Statement String -> KaosM String
coreCompile parses =
    runASTTransforms parses     >>=
    renameLexicals              >>=
    typecheck . astToCore       >>=
    dumpFlagged "dump-early-core" dumpCore >>=
    targExpand                  >>=
    dumpFlagged "dump-final-core" dumpCore >>=
    performFolding              >>=
    dumpFlagged "dump-folded-core" dumpCore >>=
    markAccess                  >>=
    dumpFlagged "dump-access-core" dumpCore >>=
    markFuture                  >>=
    markStorage                 >>=
    dumpFlagged "dump-marked-core" dumpCore >>=
    coreToVirt                  >>=
    regAlloc                    >>=
    return . emitCaos

runCompile :: [String] -> Statement String -> IO String
runCompile flags = runKaosM flags . coreCompile

doCompile :: Settings -> IO ()
doCompile s = do
    when ((length $ sourceFiles s) == 0) $ fail "No source files"
    sourceHandles <- mapM openSourceFile $ sourceFiles s
    parses <- mapM parseFile sourceHandles
    let merged = head parses
    putStrLn =<< runCompile (debugFlags s) merged

parseFile :: (String, Handle) -> IO (Statement String)
parseFile (name, handle) = do
    contents <- hGetContents handle
    let result = runParser parser () name contents
    case result of
        Right x -> return x
        Left e -> do
            hPutStrLn stderr "Parse error:"
            hPrint stderr e
            exitFailure
