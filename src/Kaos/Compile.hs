module Kaos.Compile (coreCompile) where

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

coreCompile :: Statement String -> KaosM String
coreCompile parses =
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

