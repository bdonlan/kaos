{-# OPTIONS_GHC -fno-monomorphism-restriction #-}
module Kaos.ASTTransforms (runASTTransforms) where

-- Misc AST transforms
-- Prior to renaming for now

import Kaos.AST
import Data.Generics
import Kaos.KaosM
import Control.Monad
import Debug.Trace

condDepth :: (Show t) => BoolExpr t -> Int
condDepth (BCompare _ _ _) = 0
condDepth (BAnd e1 e2) = succ $ liftM2 max ($e1) ($e2) condDepth
condDepth (BOr  e1 e2) = succ $ liftM2 max ($e1) ($e2) condDepth
condDepth e            = 0 -- error $ "not in normal form: " ++ show e

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

runASTTransforms :: Statement String -> KaosM (Statement String)
runASTTransforms =
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

        invertC CLT = CGE
        invertC CLE = CGT
        invertC CEQ = CNE
        invertC CNE = CEQ
        invertC CGT = CLE
        invertC CGE = CLT

