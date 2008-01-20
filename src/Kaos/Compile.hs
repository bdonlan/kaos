module Kaos.Compile (compile) where

import Control.Monad.State

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Sequence as S
import qualified Data.Map as M
import Data.Sequence ( (|>), (><), ViewL(..) )
import Data.ByteString.Char8 (ByteString)

import Kaos.AST
import Kaos.ASTToCore
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
import Kaos.Typecheck
import Kaos.Toplevel
import Kaos.KaosM

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

compileCode :: Statement String -> CompileM String
compileCode = lift . compileCode'

compileCode' :: Statement String -> KaosM String
compileCode' parses =
    runASTTransforms parses     >>=
    renameLexicals              >>=
    typecheck . astToCore       >>=
    dumpFlagged "dump-early-core" dumpCore  >>=
    targExpand                  >>=
    dumpFlagged "dump-final-core" dumpCore  >>=
    unlessSet "no-folding" performFolding   >>=
    dumpFlagged "dump-folded-core" dumpCore >>=
    markAccess                  >>=
    dumpFlagged "dump-access-core" dumpCore >>=
    markFuture                  >>=
    markStorage                 >>=
    dumpFlagged "dump-marked-core" dumpCore >>=
    coreToVirt                  >>=
    regAlloc                    >>=
    return . emitCaos

data CompileState = CS  { csInstallBuffer   :: S.Seq ByteString
                        , csScriptBuffer    :: S.Seq ByteString
                        , csRemoveBuffer    :: S.Seq ByteString
                        , csDefinedMacros   :: M.Map String Macro
                        }
type CompileM = StateT CompileState KaosM
csEmpty :: CompileState
csEmpty = CS S.empty S.empty S.empty M.empty


emitInstall :: String -> CompileM ()
emitInstall iss = modify $
    \s -> s { csInstallBuffer = csInstallBuffer s S.|> BS.pack iss }
emitScript :: String -> CompileM ()
emitScript iss = modify $
    \s -> s { csScriptBuffer = csScriptBuffer s S.|> BS.pack iss }
emitRemove :: String -> CompileM ()
emitRemove iss = modify $
    \s -> s { csRemoveBuffer = csRemoveBuffer s S.|> BS.pack iss }


compileUnit :: KaosUnit -> CompileM ()
compileUnit (InstallScript s) = compileCode s >>= emitInstall
compileUnit (RemoveScript s)  = compileCode s >>= emitRemove
compileUnit (AgentScript fmly gnus spcs scrp code) = do
    emitScript $ "SCRP " ++ (show fmly) ++ " " ++ (show gnus) ++ " " ++
                 (show spcs) ++ " " ++ (show scrp) ++ "\n"
    compileCode code >>= emitScript
    emitScript "ENDM\n"
compileUnit (MacroBlock macro) = do
    s <- get
    let inCtx = macro $ csDefinedMacros s
    put $ s { csDefinedMacros = M.insert (mbName inCtx) inCtx (csDefinedMacros s) }

prepSeq :: CompileState -> S.Seq ByteString
prepSeq cs =
    (csInstallBuffer cs) >< ((csScriptBuffer cs) |> (BS.pack "RSCR\n")) ><
    (csRemoveBuffer cs)

seq2lbs :: S.Seq ByteString -> LBS.ByteString
seq2lbs = LBS.fromChunks . seq2lbs'
    where
        seq2lbs' = seq2lbs'' . S.viewl
        seq2lbs'' EmptyL = []
        seq2lbs'' (s S.:< remain) = s:(seq2lbs' remain)

compile :: [String] -> KaosSource -> IO (Maybe LBS.ByteString)
compile flags code = do
    finalState <- runKaosM flags $ execStateT (mapM_ compileUnit code) csEmpty
    return $ Just (seq2lbs $ prepSeq finalState)
