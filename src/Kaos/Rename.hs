module Kaos.Rename (renameLexicals) where

import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe

import Kaos.AST
import Kaos.Slot
import Kaos.Toplevel

import Kaos.KaosM

type RenameT = StateT (M.Map String Slot) (ReaderT MacroContext KaosM)

renameLexicals :: MacroContext -> Statement String -> KaosM (Statement Slot)
renameLexicals ctx st =
    runReaderT (evalStateT (renameStatement st) M.empty) ctx

renameStatement :: Statement String -> RenameT (Statement Slot)
renameStatement (SBlock l)      = fmap SBlock $ mapM renameStatement l
renameStatement (SExpr e)       = fmap SExpr  $ renameExpr e
renameStatement (SCond c s1 s2) =
    liftM3 SCond (renameCond c) (renameStatement s1) (renameStatement s2)
renameStatement (SDoUntil c s) =
    liftM2 SDoUntil (renameCond c) (renameStatement s)
renameStatement (SUntil c s) =
    liftM2 SUntil (renameCond c) (renameStatement s)
renameStatement (SICaos l) = fmap SICaos $ mapM renameILine l
renameStatement (SInstBlock s) = liftM SInstBlock $ renameStatement s

renameILine :: InlineCAOSLine String -> RenameT (InlineCAOSLine Slot)
renameILine (ICAssign v1 v2) = liftM2 ICAssign (lex2slot v1) (lex2slot v2)
renameILine (ICConst v1 cv) = do
    s <- lex2slot v1
    return $ ICConst s cv
renameILine (ICLine tl) = liftM ICLine $ mapM renameIToken tl
renameILine (ICTargReader vs body) =
    liftM2 ICTargReader (lex2slot vs) (mapM renameILine body)
renameILine (ICTargWriter vs body) =
    liftM2 ICTargWriter (lex2slot vs) (mapM renameILine body)

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
renameExpr (ELexical l) = fmap ELexical $ lex2slot l
renameExpr (EBoolCast e) = liftM EBoolCast $ renameCond e

--renameExpr (ECall s e) = fmap (ECall s) $ mapM renameExpr e
renameExpr (ECall name e) = do
    macroM <- asks ($name)
    macro <- case macroM of
        Nothing -> fail $ "Unknown macro name \"" ++ name ++ "\""
        Just  m -> return m
    instd <- mapM instExpr e
    let expSlots = map fst instd
    let oprefix  = map snd instd
    (iprefix, newMap) <- instantiateVars [] M.empty (mbArgs macro) expSlots
    oldMap <- get
    put $ newMap
    inner <- local (const $ mbContext macro) $ renameStatement $ (SBlock $ iprefix ++ [mbCode macro])
    newMap' <- get
    put oldMap
    return $ EStmt (M.lookup "_return" newMap') (SBlock (oprefix ++ [inner]))
    where
        instExpr :: Expression String -> RenameT (Slot, Statement Slot)
        instExpr expr = do
            slot <- newSlot
            rexpr <- renameExpr expr
            return (slot, SExpr $ EAssign (ELexical slot) rexpr)
        instantiateVars prefix argMap [] [] = return (prefix, argMap)
        instantiateVars _ _ [] (_:_) = fail $ "Too many args for macro '" ++ name ++ "'"
        instantiateVars prefix argMap (harg:args) []
            | isJust $ maDefault harg
            = instantiateVars (constSetter:prefix) argMap args []
            | otherwise
            = fail $ "Too few args for macro '" ++ name ++ "'"
            where
                constSetter = SExpr (EAssign (ELexical (maName harg)) (EConst (fromJust $ maDefault harg)))
        instantiateVars prefix argMap (harg:args) (hexp:exps) = do
            instantiateVars prefix (M.insert (maName harg) hexp argMap) args exps

renameExpr (EStmt result code) = liftM2 EStmt (liftMaybe lex2slot result) (renameStatement code)

liftMaybe :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
liftMaybe _ Nothing  = return Nothing
liftMaybe f (Just v) = liftM Just $ f v 

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
