{-# OPTIONS_GHC -fno-monomorphism-restriction #-}
module Kaos.ASTTransforms (preRenameTransforms, postRenameTransforms) where

-- Misc AST transforms

import Control.Monad

import Data.Generics
import Data.Maybe

import Kaos.AST
import Kaos.KaosM
import Kaos.Slot

condDepth :: (Show t) => BoolExpr t -> Int
condDepth (BCompare _ _ _) = 0
condDepth (BAnd e1 e2) = succ $ liftM2 max ($e1) ($e2) condDepth
condDepth (BOr  e1 e2) = succ $ liftM2 max ($e1) ($e2) condDepth
condDepth _            = 0 -- error $ "not in normal form: " ++ show e

balanceConds :: Statement String -> Statement String
balanceConds = everywhere (mkT balanceLocal)
    where
        balanceLocal :: BoolExpr String -> BoolExpr String
        balanceLocal (BAnd e1 e2) = b' BAnd e1 e2
        balanceLocal (BOr e1 e2)  = b' BOr  e1 e2
        balanceLocal e            = e

        b' t e1 e2
            | condDepth e2 > condDepth e1
            = t e2 e1
            | otherwise
            = t e1 e2

expandConds :: Statement String -> Statement String
expandConds = everywhere (mkT expLocal)
    where
        expLocal :: BoolExpr String -> BoolExpr String
        expLocal (BAnd e1 e2)
            | condDepth e2 /= 0
            = BAnd e1 (BExpr (EBoolCast e2))
        expLocal (BOr e1 e2)
            | condDepth e2 /= 0
            = BOr e1 (BExpr (EBoolCast e2))
        expLocal e = e

preRenameTransforms :: Statement String -> KaosM (Statement String)
preRenameTransforms =
        check .
        expandCasts .
        expandConds .
        balanceConds .
        foldNots .
        expandCasts .
        foldCasts .
        id
    where
        check = everywhereM (mkM checkBool)
        checkBool :: BoolExpr String -> KaosM (BoolExpr String)
        checkBool e@(BNot _) = fail $ "assert: checkNot fail " ++ show e
        checkBool e@(BExpr _) = fail $ "assert$ checkExpr fail " ++ show e
        checkBool x = return x

foldCasts :: (Typeable a, Data a) => a -> a
foldCasts = everywhere (mkT foldLocalB . mkT foldLocalE)
    where
        foldLocalB :: BoolExpr String -> BoolExpr String
        foldLocalB (BExpr (EBoolCast e)) = e
        foldLocalB e = e
        foldLocalE :: Expression String -> Expression String
        foldLocalE (EBoolCast (BExpr e)) = e
        foldLocalE e = e

expandCasts :: (Typeable a, Data a) => a -> a
expandCasts = everywhere (mkT expandCast)
    where
        expandCast :: BoolExpr String -> BoolExpr String
        expandCast (BExpr e) = BCompare CNE e (EConst (CInteger 0))
        expandCast e = e

foldNots :: (Typeable a, Data a) => a -> a
foldNots = everywhere (mkT foldNot)
    where
        foldNot :: BoolExpr String -> BoolExpr String
        foldNot (BNot e) = invert e
        foldNot e = e
        
        invert (BAnd e1 e2) = BOr  (invert e1) (invert e2)
        invert (BOr  e1 e2) = BAnd (invert e1) (invert e2)
        invert (BNot e)     = e
        invert (BCompare c e1 e2) = BCompare (invertC c) e1 e2
        invert e@(BExpr _)  = error $ "Internal error: Non-normal form in foldNots: " ++ show e

        invertC CLT = CGE
        invertC CLE = CGT
        invertC CEQ = CNE
        invertC CNE = CEQ
        invertC CGT = CLE
        invertC CGE = CLT

postRenameTransforms :: Statement Slot -> KaosM (Statement Slot)
postRenameTransforms code = flattenInst =<< flattenSExpr code

flattenSExpr :: Statement Slot -> KaosM (Statement Slot)
flattenSExpr = return . everywhere (mkT flatten)
    where
        flatten :: Statement Slot -> Statement Slot
        flatten (SExpr (EStmt _ s)) = s
        flatten s = s

flattenInst :: Statement Slot -> KaosM (Statement Slot)
flattenInst = return . scanInst
    where
        scanInst :: Data b => b -> b
        scanInst t
            | isNothing stmt 
            = gmapT scanInst t
            | otherwise
            = extT id scanInst' t
            where
                stmt :: Maybe (Statement Slot)
                stmt = cast t
        scanInst' :: Statement Slot -> Statement Slot
        scanInst' (SInstBlock s) = SInstBlock $ everywhere (mkT dropInst) s
        scanInst' s = gmapT scanInst s
        dropInst :: Statement Slot -> Statement Slot
        dropInst (SInstBlock s) = s
        dropInst s = s

