module Kaos.CoreToVirt (coreToVirt) where

import Kaos.AST
import Kaos.Core
import Kaos.VirtRegister
import Kaos.Slot
import Kaos.KaosM
import Kaos.CAOS

import Kaos.CoreFuture
import Kaos.CoreStorage

import Data.List
import Data.Maybe
import Debug.Trace
import Control.Monad.State hiding (State)
import Control.Monad.Reader

import qualified Data.Map as M

type TransM a = ReaderT StorageS KaosM a

lookupStorage slot = asks (M.lookup slot . getSM)
lookupFuture  slot = asks (M.lookup slot . getFuture)

coreToVirt :: Core StorageS -> KaosM (CAOS VirtRegister)
coreToVirt b = runReaderT (transBlock b) undefined

transBlock :: CoreBlock StorageS -> TransM (CAOSBlock VirtRegister)
transBlock (CB l) = fmap concat $ mapM transLine_ l
    where
        transLine_ :: (CoreLine StorageS, StorageS) -> TransM [CAOSLine VirtRegister]
        transLine_ (line, env) = local (const env) (transLine line)

transLine :: CoreLine StorageS -> TransM [CAOSLine VirtRegister]
transLine (CoreNote n) = return []
transLine (CoreTouch _) = return []
transLine line@(CoreLine l) = do
        tokens <- mapM transToken l
        return [CAOSLine tokens]
    where
        transToken (TokenLiteral l) = return $ CAOSLiteral l
        transToken (TokenConst c) = return $ CAOSConst c
        transToken (TokenSlot (SA slot _)) = lookupStorage slot >>= transStorage
        transStorage (Just (Private r)) = return $ CAOSRegister r
        transStorage (Just (Shared r)) = return $ CAOSRegister r
        transStorage (Just (Const c)) = return $ CAOSConst c
        transStorage x = fail $ "transLine: register storage in bad state: " ++ show line
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
        checkAssign _ _ _ Nothing = return [] -- rename
        checkAssign (Just (Shared _)) _ _ _ = return [] -- alias
        checkAssign _ Nothing _ _ = return [] -- unused
        checkAssign (Just (Private r)) _ (Just srcStorage) _ = doAssign r srcStorage

        checkAssign _ _ _ _ = fail $ "checkAssign: impossible state " ++ show l
        doAssign r (Private s) =
            doAssignType (slotType dest) (CAOSRegister r) (CAOSRegister s)
        doAssign r (Shared s) =
            doAssignType (slotType dest) (CAOSRegister r) (CAOSRegister s)
        doAssign r (Const s) =
            doAssignType (slotType dest) (CAOSRegister r) (CAOSConst s)

transLine l@(CoreCond cond iftrue iffalse) = do
    cond' <- transLine (CoreLine $ (TokenLiteral "doif"):cond)
    iftrue' <- transBlock iftrue
    iffalse' <- transBlock iffalse
    return $ cond'
          ++ iftrue'
          ++ [CAOSLine $ [CAOSLiteral "else"]]
          ++ iffalse'
          ++ [CAOSLine $ [CAOSLiteral "endi"]]

transLine l@(CoreLoop body ) = transBlock body

doAssignType :: CAOSType -> CAOSToken a -> CAOSToken a -> TransM (CAOS a)
doAssignType t dest src
    | length clauses == 0
    = fail "Void type in assignment"
    | length clauses == 1
    = let [(_, body)] = clauses
      in  return [body]
    | otherwise
    = do let (hcond, hbody):t = clauses
         return $ [ CAOSLine ((CAOSLiteral "doif"):hcond), hbody] ++ fmt t ++
                  [ CAOSLine [CAOSLiteral "endi"] ]
    where
        fmt [] = []
        fmt ((hcond, hbody):t) =
            [ CAOSLine ((CAOSLiteral "elif"):hcond), hbody ] ++ fmt t
        allverbs =
            [(typeNum, ([CAOSLiteral "le", CAOSConst $ CInteger 1], "setv"))
            ,(typeStr, ([CAOSLiteral "eq", CAOSConst $ CInteger 2], "sets"))
            ,(typeObj, ([CAOSLiteral "eq", CAOSConst $ CInteger 3], "seta"))]
        possverbs = filter ((/= typeVoid) . (typeAnd t) . fst) allverbs
        clauses = map makeclause possverbs
        prelude = [CAOSLiteral "TYPE", src]
        makeclause (_, (cond, verb)) =
            (prelude ++ cond, CAOSLine [CAOSLiteral verb, dest, src])


