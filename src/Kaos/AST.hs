{-# OPTIONS -fglasgow-exts #-}

{-
    Kaos - Kaos compiler
    Copyright (C) 2005  Bryan Donlan

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}
module Kaos.AST (
            ConstValue(..), Expression(..), Statement(..),
            CAOSType(ctNum, ctStr, ctObj), typeAnd, typeOr,
            typeAny, typeNum, typeStr, typeObj, typeVoid,
            constType, comparisonToCAOS, BoolExpr(..),
            Comparison(..), AccessType(..),
            InlineCAOSToken(..), InlineCAOSLine(..),
            ) where

import Data.List
import Data.Generics
import Kaos.PrettyM

data AccessType = NoAccess | ReadAccess | WriteAccess | MutateAccess
    deriving (Show, Ord, Eq, Data, Typeable)

data ConstValue =
    CString  String
  | CInteger Int
  | CFloat   Double
  deriving (Eq, Ord, Data, Typeable)

instance Show ConstValue where
    show (CString s) = "#<s" ++ s ++ ">"
    show (CInteger i) = "#<i" ++ (show i) ++ ">"
    show (CFloat f) = "#<f" ++ (show f) ++ ">"

data Comparison = CLT | CEQ | CLE | CGT | CGE | CNE
    deriving (Eq, Ord, Show, Data, Typeable)

comparisonToCAOS :: Comparison -> String
comparisonToCAOS CLT = "lt"
comparisonToCAOS CEQ = "eq"
comparisonToCAOS CLE = "le"
comparisonToCAOS CGT = "gt"
comparisonToCAOS CGE = "ge"
comparisonToCAOS CNE = "ne"

data BoolExpr l =
    BAnd  (BoolExpr l) (BoolExpr l)
  | BOr   (BoolExpr l) (BoolExpr l)
  | BNot  (BoolExpr l)              -- non-normal form
  | BExpr (Expression l)            -- non-normal form
  | BCompare Comparison (Expression l) (Expression l)
  deriving (Eq, Ord, Show, Data, Typeable)

data Expression l =
    EConst ConstValue
  | EBinaryOp String (Expression l) (Expression l)
  | ELexical l
  | EAssign (Expression l) (Expression l)
  | ECall String [Expression l]
  | EStmt (Maybe l) (Statement l)
  | EBoolCast (BoolExpr l)
  deriving (Eq, Ord, Data, Typeable)

instance Show l => Show (Expression l) where
    show (EConst c) = show c
    show (EBinaryOp s e1 e2) = "o:" ++ s ++ (show (e1, e2))
    show (ELexical l) = "l:" ++ show l
    show (EAssign e1 e2) = "assign:" ++ show (e1, e2)
    show (ECall s e) = "call:" ++ s ++ show e
    show (EBoolCast c) = "bcast:" ++ (show c)
    show (EStmt l s) = "stmt:" ++ (show l) ++ ":" ++ show s

data Statement l =
    SExpr       (Expression l)
  | SBlock      [Statement l] 
  | SDoUntil    (BoolExpr l) (Statement l)
  | SUntil      (BoolExpr l) (Statement l)
  | SCond       (BoolExpr l) (Statement l) (Statement l)
  | SICaos      [InlineCAOSLine l]
  | SInstBlock  (Statement l)
    deriving (Eq, Ord, Data, Typeable)

data InlineCAOSLine l =
    ICAssign l l
  | ICConst l ConstValue
  | ICLine [InlineCAOSToken l]
  | ICTargReader l [InlineCAOSLine l]
  | ICTargWriter l [InlineCAOSLine l]
    deriving (Eq, Ord, Data, Typeable)

data InlineCAOSToken l =
    ICVar l AccessType
  | ICWord String
    deriving (Eq, Ord, Data, Typeable)

prettyStatement :: Show t => Statement t -> PrettyM ()
prettyStatement (SExpr e) = emitLine $ (show e) ++ ";"
prettyStatement (SBlock b) = do
    emitLine "{"
    withIndent 2 $ mapM_ prettyStatement b
    emitLine "}"
prettyStatement x = emitLine $ show x -- XXX

instance Show l => Show (Statement l) where
    show = runPretty . prettyStatement

data CAOSType = CAOSType { ctNum :: Bool
                         , ctStr :: Bool
                         , ctObj :: Bool
                         }
                         deriving (Eq, Ord, Data, Typeable)

instance Show CAOSType where
    show t = "<type:" ++ ts ++ ">"
        where
            ts
                | t == typeVoid
                = "void"
                | otherwise
                = concat $ intersperse "|" typeStrs
            typeStrs = map snd $ filter (typeMatches t . fst) typeNames
            typeNames = [(typeNum, "numeric"),
                         (typeStr, "string"),
                         (typeObj, "object")]

typeAnd :: CAOSType -> CAOSType -> CAOSType
typeAnd (CAOSType a b c) (CAOSType a' b' c')
    = CAOSType (a && a') (b && b') (c && c')
typeOr :: CAOSType -> CAOSType -> CAOSType
typeOr (CAOSType a b c) (CAOSType a' b' c')
    = CAOSType (a || a') (b || b') (c || c')

typeMatches :: CAOSType -> CAOSType -> Bool
typeMatches a b = typeVoid /= (a `typeAnd` b)

typeAny :: CAOSType
typeAny = CAOSType True True True
typeNum :: CAOSType
typeNum = CAOSType True False False
typeStr :: CAOSType
typeStr = CAOSType False True False
typeObj :: CAOSType
typeObj = CAOSType False False True
typeVoid :: CAOSType
typeVoid = CAOSType False False False

constType :: ConstValue -> CAOSType
constType (CInteger _) = typeNum
constType (CFloat _) = typeNum
constType (CString _) = typeStr
