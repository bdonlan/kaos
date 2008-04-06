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

module Kaos.RegAlloc (regAlloc) where

import Kaos.AST
import Kaos.CAOS
import Kaos.VirtRegister
import Kaos.KaosM

import Data.Generics
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Array.Unboxed
import Data.List
import Data.Maybe
import Data.Char

type RAM t = StateT RAS KaosM t
data RAS = RAS { rasUseCount :: M.Map VirtRegister Int
               , rasActive   :: M.Map VirtRegister CAOSRegister
               , rasUnused   :: [CAOSRegister]
               , rasZeroed   :: UArray CAOSRegister Bool
               , rasLocal    :: S.Set VirtRegister
               , rasDead     :: S.Set VirtRegister
               , rasInLoop   :: Bool
               }

reap :: RAM ()
reap = do
    s <- get
    let zombies = (rasDead s) `S.difference` (rasLocal s)
    let dead    = S.toList $ (rasLocal s) `S.intersection` (rasDead s)
    let deadReg = map (fromJust . (flip M.lookup $ rasActive s)) dead
    whenSet "trace-reg-alloc" $ mapM_ (\r -> debugKM $ "Kill " ++ show r) (zip dead deadReg)
    put $ s { rasDead   = zombies
            , rasUnused = (rasUnused s) ++ deadReg
            , rasActive = foldl' (flip M.delete) (rasActive s) dead
            }

enterBlock :: RAM a -> RAM a
enterBlock m = do
    s <- get
    put $ s { rasLocal = S.empty }
    r <- m
    modify $ \s' -> s' { rasLocal = rasLocal s `S.union` rasLocal s' }
    reap
    return r

enterLoop :: RAM a -> RAM a
enterLoop m = do
    s <- get
    put $ s { rasInLoop = True }
    r <- m
    modify $ \s' -> s' { rasInLoop = rasInLoop s }
    return r

rasInit :: M.Map VirtRegister Int -> RAS
rasInit m = RAS { rasUseCount = m
                , rasActive   = M.empty
                , rasUnused   = map CAOSReg [0..99]
                , rasZeroed   = accumArray undefined True (CAOSReg 0, CAOSReg 99) []
                , rasLocal    = S.empty
                , rasDead     = S.empty
                , rasInLoop   = False
                }

useUp :: VirtRegister -> RAM ()
useUp vr = do
    s <- get
    let uc = M.alter (\(Just n) -> guard (n > 1) >> return (pred n)) vr (rasUseCount s)
    put $ s { rasUseCount = uc }
    when (M.notMember vr uc) $ modify (\s' -> s' { rasDead = S.insert vr (rasDead s) })

vivify :: VirtRegister -> RAM CAOSRegister
vivify vr = do
    s <- get
    case rasUnused s of
        [] -> fail "Out of CAOS registers"
        reg:remain -> do
            put $ s { rasActive = M.insertWith (error "vivify on live reg") vr reg (rasActive s)
                    , rasUnused = remain
                    , rasZeroed = rasZeroed s // [(reg, False)]
                    , rasLocal  = S.insert vr (rasLocal s)
                    }
            whenSet "trace-reg-alloc" $ debugKM ("Live: " ++ (show (vr, reg)))
            return reg

translate :: VirtRegister -> RAM CAOSRegister
translate vr = do
    s <- get
    when (M.notMember vr (rasUseCount s)) $ fail ("Zombie register referenced: " ++ show vr)
    reg <- if (M.notMember vr (rasActive s)) 
        then vivify vr
        else M.lookup vr (rasActive s)
    useUp vr
    return reg

countAccess :: CAOS VirtRegister -> M.Map VirtRegister Int
countAccess = foldl' markOne M.empty . listify predicate
    where
        predicate :: VirtRegister -> Bool
        predicate = const True
        markOne m vr = M.insertWith (+) vr 1 m

regAlloc :: CAOS VirtRegister -> KaosM (CAOS CAOSRegister)
regAlloc l = evalStateT (mapM regLine l) (rasInit $ countAccess l)

regLine :: CAOSLine VirtRegister -> RAM (CAOSLine CAOSRegister)
-- special case to eliminate excess SETV VAxx 0
regLine (CAOSLine l@[CAOSLiteral w, CAOSRegister _, CAOSConst (CInteger 0)])
    | map toUpper w == "SETV"
    = do
        s <- get
        std@(CAOSLine [_,CAOSRegister vr,_]) <-
            enterBlock $ liftM CAOSLine (mapM regTok l)
        case (rasInLoop s, (rasZeroed s) ! vr) of
            (False, True) -> return $ CAOSLine []
            _             -> return std
regLine (CAOSLine l) = enterBlock $ liftM CAOSLine (mapM regTok l)
regLine (CAOSLoop l) = enterLoop  $ liftM CAOSLoop (mapM regLine l)

regTok :: CAOSToken VirtRegister -> RAM (CAOSToken CAOSRegister)
regTok (CAOSRegister vr) = liftM CAOSRegister (translate vr)
regTok (CAOSLiteral l) = return $ CAOSLiteral l
regTok (CAOSConst c) = return $ CAOSConst c
