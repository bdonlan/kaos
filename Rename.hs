module Rename (renameLexicals) where

import Control.Monad.State
import AST
import Slot

renameLexicals :: (Eq n, Ord n) => Statement n -> Statement VirtRegister
renameLexicals = evalStateT . runVRegAllocT . renameStatement

renameStatement (SBlock l) = fmap SBlock $ mapM renameStatement l
renameStatement (SExpr e)  = fmap SExpr  $ renameExpr e

renameExpr (EConst c) = EConst c
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
            slot <- newSlot
            put $ M.insert l slot s
            return $ ELexical slot
