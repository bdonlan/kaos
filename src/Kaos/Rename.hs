module Kaos.Rename (renameLexicals) where

import Control.Monad.State
import Kaos.AST
import Kaos.Slot
import qualified Data.Map as M

import Kaos.KaosM

type RenameT = StateT (M.Map String Slot) KaosM

renameLexicals :: Statement String -> KaosM (Statement Slot)
renameLexicals = flip evalStateT M.empty . renameStatement

renameStatement :: Statement String -> RenameT (Statement Slot)
renameStatement (SBlock l)      = fmap SBlock $ mapM renameStatement l
renameStatement (SExpr e)       = fmap SExpr  $ renameExpr e
renameStatement (SCond c s1 s2) =
    liftM3 SCond (renameCond c) (renameStatement s1) (renameStatement s2)
renameStatement (SDoUntil c s) =
    liftM2 SDoUntil (renameCond c) (renameStatement s)
renameStatement (SICaos l) = fmap SICaos $ mapM renameILine l

renameILine :: InlineCAOSLine String -> RenameT (InlineCAOSLine Slot)
renameILine (ICAssign v1 v2) = liftM2 ICAssign (lex2slot v1) (lex2slot v2)
renameILine (ICConst v1 cv) = do
    s <- lex2slot v1
    return $ ICConst s cv
renameILine (ICLine tl) = liftM ICLine $ mapM renameIToken tl

renameIToken :: InlineCAOSToken String -> RenameT (InlineCAOSToken Slot)
renameIToken (ICVar l at) = do
    s <- lex2slot l
    return $ ICVar s at
renameIToken (ICWord w) = return $ ICWord w

renameExpr :: (Expression String) -> RenameT (Expression Slot)
renameExpr (EConst c) = return $ EConst c
renameExpr (EBinaryOp s e1 e2) =
    liftM2 (EBinaryOp s) (renameExpr e1) (renameExpr e2)
renameExpr (EAssign e1 e2) =
    liftM2 EAssign (renameExpr e1) (renameExpr e2)
renameExpr (ECall s e) = fmap (ECall s) $ mapM renameExpr e
renameExpr (ELexical l) = fmap ELexical $ lex2slot l
renameExpr (EBoolCast e) = liftM EBoolCast $ renameCond e

lex2slot :: String -> RenameT Slot
lex2slot l = do
    s <- get
    case M.lookup l s of
        Just v -> return v
        Nothing -> do
            slot <- lift $ newSlot
            put $ M.insert l slot s
            return slot

renameCond :: BoolExpr String -> RenameT (BoolExpr Slot)
renameCond (BAnd b1 b2) = liftM2 BAnd (renameCond b1) (renameCond b2)
renameCond (BOr b1 b2) = liftM2 BOr (renameCond b1) (renameCond b2)
renameCond (BCompare c e1 e2) =
    liftM2 (BCompare c) (renameExpr e1) (renameExpr e2)
renameCond e = fail $ "non-normal form in renameCond: " ++ show e
