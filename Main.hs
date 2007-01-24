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

import AST
import Core
import CAOS
import KaosM
import Slot
import VirtRegister

import Parser
import Rename
import ASTToCore
import CoreToVirt
import RegAlloc
import Emit

import Debug.Trace

data Settings = Settings {
    helpOnly :: Bool,
    versionOnly :: Bool,
    sourceFiles :: [String],
    outputFile :: String
    }

instance Show Settings where
    show s = unlines $
        ["helpOnly:    " ++ (show $ helpOnly    s)
        ,"versionOnly: " ++ (show $ versionOnly s)
        ,"sourceFiles: " ++ (show $ sourceFiles s)
        ,"outputFile:  " ++ (show $ outputFile  s)
        ]
        

defaults = Settings {
    helpOnly = False,
    versionOnly = False,
    sourceFiles = [],
    outputFile = ""
    }

parseArgs s ("--help":_) = return $ s { helpOnly = True }
parseArgs s ("--version":_) = return $ s { versionOnly = True }
parseArgs s ((a@('-':'-':_)):_) = fail $ "Unknown argument " ++ a
parseArgs s (('-':t@(_:_)):remain) = parseShortArgs s t remain

parseArgs s (file:tail) =
    parseArgs (s { sourceFiles = file:sourceFiles s }) tail

parseArgs s [] = return s

setOutputFile s "" = fail "Cannot use an empty string as an output file"

setOutputFile s file =
    case outputFile s of
        "" -> return $ s { outputFile = file }
        _  -> fail "Can't set output file twice"

parseShortArgs s ('h':_) _ = return $ s { helpOnly = True }
parseShortArgs s ('v':_) _ = return $ s { versionOnly = True }

parseShortArgs s ('o':file@(_:_)) remain =
    do  s <- setOutputFile s file
        parseArgs s remain

parseShortArgs s ('o':[]) (file:remain) =
    do  s <- setOutputFile s file
        parseArgs s remain

parseShortArgs s ('o':[]) [] = fail "-o requires an argument"
parseShortArgs s (x:_) _ = fail $ "Unknown short argument -" ++ [x]

versionStr = "HaKaos v0.0"
helpStr = unlines $ versionStr:
    ["Usage: kaos [-o file] [--help] [--version] file [...]"
    ,""
    ,"Arguments:"
    ,"-o file   Write output CAOS to this file"
    ,"--help    Shows this text"
    ,"--version Show program version"
    ]

setDefaultOutputFile s =
    case outputFile s of
        "" -> s { outputFile = "output.cos" }
        _  -> s

main = do
    argv <- getArgs
    s <- parseArgs defaults argv
    let s' = setDefaultOutputFile s
    if helpOnly s'
        then putStr helpStr
        else if versionOnly s'
            then putStrLn versionStr
            else doCompile s'

openSourceFile "-" = return ("(stdin)", stdin)
openSourceFile file = do
    h <- openFile file ReadMode
    return (file, h)

coreCompile :: Statement String -> KaosM String
coreCompile parses =
    renameLexicals parses   >>=
    astToCore               >>= \c -> trace ("core: " ++ show c) $
    coreToVirt c            >>=
    regAlloc                >>=
    return . emitCaos

runCompile = runKaosM . coreCompile

doCompile s = do
    when ((length $ sourceFiles s) == 0) $ fail "No source files"
    sourceHandles <- mapM openSourceFile $ sourceFiles s
    parses <- mapM parseFile sourceHandles
    let merged = head parses
    putStrLn $ show merged
    putStrLn =<< runCompile merged

parseFile (name, handle) = do
    contents <- hGetContents handle
    let result = runParser parser () name contents
    case result of
        Right x -> return x
        Left e -> do
            hPutStrLn stderr "Parse error:"
            hPrint stderr e
            exitFailure

writeOpstream "-" o = putStr $ unlines o
writeOpstream file o = do
    handle <- openFile file WriteMode
    hPutStr handle $ unlines o
    hClose handle
