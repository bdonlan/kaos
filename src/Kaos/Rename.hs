module Kaos.Rename (renameLexicals) where

import Control.Monad.State
import Kaos.AST
import Kaos.Slot
import Data.Generics
import qualified Data.Map as M

import Kaos.KaosM

type RenameT = StateT (M.Map String Slot) KaosM

renameLexicals :: Statement String -> KaosM (Statement Slot)
renameLexicals = flip evalStateT M.empty . renameStatement

renameStatement (SBlock l)      = fmap SBlock $ mapM renameStatement l
renameStatement (SExpr e)       = fmap SExpr  $ renameExpr e
renameStatement (SCond c s1 s2) =
    liftM3 SCond (renameCond c) (renameStatement s1) (renameStatement s2)

renameExpr :: (Expression String) -> RenameT (Expression Slot)
renameExpr (EConst c) = return $ EConst c
renameExpr (EBinaryOp s e1 e2) =
    liftM2 (EBinaryOp s) (renameExpr e1) (renameExpr e2)
renameExpr (EAssign e1 e2) =
    liftM2 EAssign (renameExpr e1) (renameExpr e2)
renameExpr (ECall s e) = fmap (ECall s) $ mapM renameExpr e
renameExpr (ELexical l) = do
    s <- get
    case M.lookup l s of
        Just v -> return $ ELexical v
        Nothing -> do
            slot <- lift $ newSlot
            put $ M.insert l slot s
            return $ ELexical slot
renameExpr (EBoolCast e) = liftM EBoolCast $ renameCond e

renameCond (BAnd b1 b2) = liftM2 BAnd (renameCond b1) (renameCond b2)
renameCond (BOr b1 b2) = liftM2 BOr (renameCond b1) (renameCond b2)
renameCond (BCompare c e1 e2) =
    liftM2 (BCompare c) (renameExpr e1) (renameExpr e2)
renameCond e = fail $ "non-normal form in renameCond: " ++ show e
