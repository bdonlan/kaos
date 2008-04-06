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
module Kaos.CoreToVirt (coreToVirt) where

import Kaos.AST
import Kaos.Core
import Kaos.VirtRegister
import Kaos.Slot
import Kaos.KaosM
import Kaos.CAOS
import Kaos.CoreInline (inlineFallback)

import Kaos.CoreFuture
import Kaos.CoreStorage

import Data.List
import Data.Maybe
import Control.Monad.Reader

import qualified Data.Map as M

type TransM a = ReaderT StorageS KaosM a

lookupStorage :: Slot -> TransM (Maybe Storage)
lookupStorage slot = asks (M.lookup slot . getSM)
lookupFuture :: Slot -> TransM Future
lookupFuture  slot = asks (M.lookup slot . getFuture)

coreToVirt :: Core StorageS -> KaosM (CAOS VirtRegister)
coreToVirt b = runReaderT (transBlock b) undefined

transBlock :: CoreBlock StorageS -> TransM (CAOSBlock VirtRegister)
transBlock (CB l) = saveCtx $ fmap concat $ mapM transLine_ l
    where
        transLine_ :: (CoreLine StorageS, StorageS) -> TransM [CAOSLine VirtRegister]
        transLine_ (line, env) = checkpoint [] $ local (const env) (transLine line)

transLine :: CoreLine StorageS -> TransM [CAOSLine VirtRegister]
transLine (CoreNote (ContextNote ctx)) = do
    putCtx $ Just ctx
    return []
transLine (CoreNote _) = return []
transLine (CoreTouch _) = return []
transLine line@(CoreLine l) = do
        tokens <- mapM transToken l
        return [CAOSLine tokens]
    where
        transToken (TokenLiteral ts) = return $ CAOSLiteral ts
        transToken (TokenConst c) = return $ CAOSConst c
        transToken (TokenSlot (SA slot _)) = lookupStorage slot >>= transStorage slot
        transStorage _ (Just (Private r)) = return $ CAOSRegister r
        transStorage _ (Just (Shared r)) = return $ CAOSRegister r
        transStorage _ (Just (Const c)) = return $ CAOSConst c
        transStorage s Nothing = uninitialized s line
        transStorage _ _ = fail $ "transLine: register storage in bad state: " ++ show line
transLine (CoreConst slot c) = lookupStorage slot >>= checkAssign
    where
        checkAssign (Just (Private r)) = doAssign r
        checkAssign (Just (Const _))   = return []
        checkAssign Nothing            = return []
        checkAssign x                  = fail $ "doConstAssign: unexpected storage " ++ show x
        doAssign r = doAssignType (constType c) (CAOSRegister r) (CAOSConst c)
transLine l@(CoreAssign dest src) = do
    destStorage <- lookupStorage dest
    destFuture  <- lookupFuture  dest
    srcStorage  <- lookupStorage src
    srcFuture   <- lookupFuture  src
    checkAssign destStorage destFuture srcStorage srcFuture
    where
        checkAssign Nothing (Just _) Nothing _ = uninitialized src l
        checkAssign _ _ _ Nothing = return [] -- rename
        checkAssign (Just (Shared _)) _ _ _ = return [] -- alias
        checkAssign (Just (Const _)) _ _ _ = return [] -- copy const
        checkAssign _ Nothing _ _ = return [] -- unused
        checkAssign (Just (Private r)) _ (Just srcStorage) _ = doAssign r srcStorage

        checkAssign a b c d = fail $ "checkAssign: impossible state " ++ show ((a,b,c,d), l)
        doAssign r (Private s) =
            doAssignType (slotType dest) (CAOSRegister r) (CAOSRegister s)
        doAssign r (Shared s) =
            doAssignType (slotType dest) (CAOSRegister r) (CAOSRegister s)
        doAssign r (Const s) =
            doAssignType (slotType dest) (CAOSRegister r) (CAOSConst s)
        doAssign _ Phantom = error "A variable was used uninitialized, but we're currently too dumb to figure out which. Bug bd_."

transLine (CoreCond cond iftrue iffalse) = do
    cond' <- transLine (CoreLine $ (TokenLiteral "doif"):cond)
    iftrue' <- transBlock iftrue
    iffalse' <- transBlock iffalse
    return $ cond'
          ++ iftrue'
          ++ [CAOSLine $ [CAOSLiteral "else"]]
          ++ iffalse'
          ++ [CAOSLine $ [CAOSLiteral "endi"]]

transLine (CoreLoop body ) = liftM (\i -> [CAOSLoop i]) $ transBlock body
transLine l@(CoreTargReader _ _ _) = error $ "Impossible: Late targreader: " ++ show l
transLine l@(CoreTargWriter _ _) = error $ "Impossible: Late targwriter: " ++ show l
transLine (CoreFoldable _ l) = transLine l
transLine (CoreInlineFlush _) = return []
transLine l@(CoreInlineAssign _ _ ds _) = do
    future <- lookupFuture ds
    case future of
        Nothing -> return []
        Just _  -> transLine $ inlineFallback l
transLine CoreTargZap = return []

doAssignType :: CAOSType -> CAOSToken a -> CAOSToken a -> TransM (CAOS a)
doAssignType t dest src
    | length clauses == 0
    = fail "Void type in assignment"
    | length clauses == 1
    = let [(_, body)] = clauses
      in  return [body]
    | otherwise
    = do let (hcond, hbody):ts = clauses
         return $ [ CAOSLine ((CAOSLiteral "doif"):hcond), hbody] ++ fmt ts ++
                  [ CAOSLine [CAOSLiteral "endi"] ]
    where
        fmt [] = []
        fmt ((hcond, hbody):ts) =
            [ CAOSLine ((CAOSLiteral "elif"):hcond), hbody ] ++ fmt ts
        allverbs =
            [(typeNum, ([CAOSLiteral "le", CAOSConst $ CInteger 1], "setv"))
            ,(typeStr, ([CAOSLiteral "eq", CAOSConst $ CInteger 2], "sets"))
            ,(typeObj, ([CAOSLiteral "eq", CAOSConst $ CInteger 3], "seta"))]
        possverbs = filter ((/= typeVoid) . (typeAnd t) . fst) allverbs
        clauses = map makeclause possverbs
        prelude = [CAOSLiteral "TYPE", src]
        makeclause (_, (cond, verb)) =
            (prelude ++ cond, CAOSLine [CAOSLiteral verb, dest, src])

uninitialized :: Show s => Slot -> CoreLine s -> TransM a
uninitialized Slot{slotName = Just name} _ =
    compileError $ "Uninitialized variable `" ++ name ++ "'"
uninitialized slot l = do
    st <- ask
    internalError $ "Uninitialized slot " ++ (show slot) ++ " in " ++ (show l) ++ " env:\n" ++ (show st)
