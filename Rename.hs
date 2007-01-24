module Rename (renameLexicals) where

import Control.Monad.State
import AST
import Slot
import qualified Data.Map as M

import KaosM

type RenameT = StateT (M.Map String Slot) KaosM

renameLexicals :: Statement String -> KaosM (Statement Slot)
renameLexicals = flip evalStateT M.empty . renameStatement

renameStatement (SBlock l) = fmap SBlock $ mapM renameStatement l
renameStatement (SExpr e)  = fmap SExpr  $ renameExpr e

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
