module Kaos.Compile (compile) where

import Control.Monad.State

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Sequence as S
import qualified Data.Map as M
import Data.Sequence ( (|>), (><), ViewL(..) )
import Data.ByteString.Char8 (ByteString)
import Data.Maybe

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
import Kaos.Toplevel
import Kaos.CoreInline
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
compileCode code = do
    st <- get
    lift $ compileCode' (flip M.lookup $ csDefinedMacros st) code

compileCode' :: MacroContext -> Statement String -> KaosM String
compileCode' ctx parses =
    preRenameTransforms parses              >>=
    renameLexicals ctx                      >>=
    postRenameTransforms                    >>=
    astToCore                               >>=
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
    markAccess                              >>=
    dumpFlagged "dump-access-core" dumpCore >>=
    markFuture                              >>=
    markStorage                             >>=
    dumpFlagged "dump-marked-core" dumpCore >>=
    coreToVirt                              >>=
    regAlloc                                >>=
    return . emitCaos

data CompileState = CS  { csInstallBuffer   :: S.Seq ByteString
                        , csScriptBuffer    :: S.Seq ByteString
                        , csRemoveBuffer    :: S.Seq ByteString
                        , csDefinedMacros   :: M.Map String Macro
                        , csOVIdx           :: Int
                        }
type CompileM = StateT CompileState KaosM
csEmpty :: CompileState
csEmpty = CS S.empty S.empty S.empty M.empty 0


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
                                    , mbArgs    = [MacroArg "o" typeObj Nothing]
                                    , mbCode    = SICaos [ICLValue minBound "_return" atok]
                                    }
        let setMacro = defaultMacro { mbName    = "set:" ++ name
                                    , mbType    = MacroLValue
                                    , mbRetType = typeVoid
                                    , mbArgs    = [MacroArg "v" t Nothing, MacroArg "o" typeObj Nothing]
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
    s <- get
    let inCtx = macro { mbContext = flip M.lookup (csDefinedMacros s) }
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
