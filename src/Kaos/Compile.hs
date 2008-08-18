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
module Kaos.Compile (compile) where

import Control.Monad.State

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Monoid
import Data.Foldable (toList)

import Kaos.AST
import Kaos.ASTToCore
import Kaos.CAOS
import Kaos.Core
import Kaos.CoreToVirt
import Kaos.CoreAccess
import Kaos.CoreFuture
import Kaos.CoreStorage
import Kaos.CoreFold
import Kaos.Rename
import Kaos.RegAlloc
import Kaos.Emit
import Kaos.Targ
import Kaos.ASTTransforms
import Kaos.Toplevel
import Kaos.CoreInline
import Kaos.KaosM
import Kaos.PrettyM
import Kaos.VirtRegister


dumpFlagged :: String -> (t -> String) -> t -> KaosM t
dumpFlagged flag f v = do
    debugDump flag (f v)
    return v

unlessSet :: String -> (t -> KaosM t) -> (t -> KaosM t)
unlessSet flag f v = do
    flagState <- isSet flag
    if flagState
        then return v
        else f v

compileCode :: Statement String -> CompileM (Core ())
compileCode code = do
    st <- get
    lift $ compileCode' (csMacroCtx st) code

dumpStmt :: Show t => Statement t -> String
dumpStmt = runPretty . prettyStatement

compileCode' :: MacroContext -> Statement String -> KaosM (Core ())
compileCode' ctx parses = return parses     >>=
    preRenameTransforms                     >>=
    dumpFlagged "dump-ast" dumpStmt         >>=
    renameLexicals ctx                      >>=
    commitFail                              >>=
    postRenameTransforms                    >>=
    dumpFlagged "dump-final-ast" dumpStmt   >>=
    astToCore                               >>=
    commitFail                              >>=
    dumpFlagged "dump-early-core" dumpCore  >>=
    unlessSet "no-folding" performFolding   >>=
    stripFolds                              >>=
    dumpFlagged "dump-folded-core" dumpCore >>=
    unlessSet "no-inline" inlineAnalysis    >>=
    unlessSet "no-targ-expand" targExpand   >>=
    stripTarg                               >>=
    dumpFlagged "dump-final-targ" dumpCore  >>=
    unlessSet "no-inline" inlineValues      >>=
    dumpFlagged "dump-final-core" dumpCore  >>=
    commitFail

finishCompile  :: MonadKaos m => Core () -> m String
finishCompile c = finishToVirt c >>= emitVirt

finishToVirt :: MonadKaos m => Core () -> m (CAOS VirtRegister)
finishToVirt = liftK . finishToVirt'
finishToVirt' :: Core () -> KaosM (CAOS VirtRegister)
finishToVirt' c = return c                  >>=
    markAccess                              >>=
    dumpFlagged "dump-access-core" dumpCore >>=
    markFuture                              >>=
    markStorage                             >>=
    dumpFlagged "dump-marked-core" dumpCore >>=
    checkConstStorage                       >>=
    coreToVirt                              >>=
    commitFail

emitVirt :: MonadKaos m => CAOS VirtRegister -> m String
emitVirt = liftK . emitVirt'
emitVirt' :: CAOS VirtRegister -> KaosM String
emitVirt' c = return c                      >>=
    regAlloc                                >>=
    commitFail                              >>=
    return . emitCaos

data CompileState = CS  { csInstallBuffer   :: Core ()
                        , csScriptBuffer    :: [ByteString]
                        , csRemoveBuffer    :: Core ()
                        , csMacroCtx        :: MacroContext
                        , csOVIdx           :: Int
                        }
type CompileM = StateT CompileState KaosM
csEmpty :: MacroContext -> CompileState
csEmpty initMacros = CS (CB []) [] (CB []) initMacros 0

emitInstall :: Core () -> CompileM ()
emitInstall iss = modify $
    \s -> s { csInstallBuffer = csInstallBuffer s `mappend` iss }
emitScript :: String -> CompileM ()
emitScript iss = modify $
    \s -> s { csScriptBuffer = BS.pack iss : csScriptBuffer s }
emitRemove :: Core () -> CompileM ()
emitRemove iss = modify $
    \s -> s { csRemoveBuffer = csRemoveBuffer s `mappend` iss }


compileUnit :: KaosUnit -> CompileM ()
compileUnit (InstallScript s) = compileCode s >>= emitInstall
compileUnit (RemoveScript s)  = compileCode s >>= emitRemove
compileUnit (AgentScript (SContext c i) code) =
    context c $ compileUnit (AgentScript i code)
compileUnit (AgentScript blkHead code) = do
    virt <- compileCode blkHead >>= finishToVirt
    prelude <- emitVirt virt
    case virt of
        [CAOSLine [CAOSLiteral _, CAOSConst _, CAOSConst _, CAOSConst _, CAOSConst _]] -> return ()
        _ -> compileError $ "Side-effects not allowed in script classifier"
    buf <- compileCode code >>= finishCompile
    emitScript $ prelude ++ buf ++ "ENDM\n\n"
compileUnit OVDecl{ ovName = name, ovIndex = Just idx, ovType = t }
    | idx < 0 || idx > 99
    = fail $ "Object variable index " ++ show idx ++ " is out of range"
    | otherwise
    = do
        let atok   = [ICWord "AVAR", ICVar "o" ReadAccess, ICWord $ show idx]
        let setter = [ICWord verb] ++ atok ++ [ICVar "v" ReadAccess]
        let getMacro = defaultMacro { mbName    = name
                                    , mbType    = MacroRValue
                                    , mbRetType = t
                                    , mbArgs    = [MacroArg "o" typeObj]
                                    , mbCode    = SICaos [ICLValue minBound "return" atok]
                                    }
        let setMacro = defaultMacro { mbName    = "set:" ++ name
                                    , mbType    = MacroLValue
                                    , mbRetType = typeVoid
                                    , mbArgs    = [MacroArg "v" t, MacroArg "o" typeObj]
                                    , mbCode    = SICaos [ICLine setter]
                                    }
        compileUnit $ MacroBlock getMacro
        compileUnit $ MacroBlock setMacro
    where
        verb
            | t == typeNum
            = "SETV"
            | t == typeStr
            = "SETS"
            | t == typeObj
            = "SETA"
            | otherwise
            = error "Bad type for object variable definition"
compileUnit d@OVDecl{} = do
    s <- get
    let idx = csOVIdx s
    put $ s { csOVIdx = succ idx }
    when (idx >= 100) $ fail "Out of object variable indexes; specify some explicitly"
    compileUnit $ d { ovIndex = Just idx }

compileUnit (MacroBlock macro) = do
    lift $ dumpFlagged "dump-macros" show macro
    code' <- lift $ preRenameTransforms (mbCode macro)
    let macro' = macro { mbCode = code' }
    s <- get
    let inCtx = macro' { mbContext = csMacroCtx s }
    newCtx <- addMacroToCtx False inCtx (csMacroCtx s)
    put $ s { csMacroCtx = newCtx }

finalEmit :: CompileState -> KaosM BS.ByteString
finalEmit st = do
    installS <- liftM BS.pack (finishCompile $ csInstallBuffer st)
    removeS  <- liftM BS.pack (finishCompile $ csRemoveBuffer st)
    return (BS.concat $ [installS] ++ (toList (csScriptBuffer st)) ++ [rscr, removeS])
    where
        rscr = BS.pack "RSCR\n"

compile :: [String] -> KaosSource -> IO (Maybe BS.ByteString)
compile flags code = do
    runKaosM flags $ do
        ctx <- builtins
        finalEmit =<< execStateT (mapM_ compileUnit code) (csEmpty ctx)
