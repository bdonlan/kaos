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
module AST (
            ValueType, vor, vand, vSubsetOf, vall, vnone, vstring, vagent, vnumber,
            Typeable(..), ConstValue(..), Expression(..), Statement(..),
            constInt, constFloat, constString, isPolymorphic, assignForType
            ) where

import Utils

newtype ValueType = ValueType (Bool, Bool, Bool)
        deriving (Eq, Ord)

instance Show ValueType where
    show (ValueType (False, False, False)) = "<none>"
    show (ValueType (True, True, True)) = "<all>"
    show (ValueType (string, agent, number)) =
        "<" ++ join " | " tl ++ ">"
        where
            tl =
                (string ?? (["string"], [])) ++
                (agent  ?? (["agent" ], [])) ++
                (number ?? (["number"], []))

vop op (ValueType (n, a, s)) (ValueType (n', a', s'))
    = ValueType ((n `op` n'), (a `op` a'), (s `op` s'))

vor  = vop (||)
vand = vop (&&)

v1 `vSubsetOf` v2 = (v1 `vand` v2) == v1

vall  = ValueType (True, True, True)
vnone = ValueType (False, False, False)

vstring = ValueType (True, False, False)
vagent  = ValueType (False, True, False)
vnumber = ValueType (False, False, True)

isPolymorphic' (False, False, False) = False
isPolymorphic' (True, False, False)  = False
isPolymorphic' (False, True, False)  = False
isPolymorphic' (False, False, True)  = False
isPolymorphic' _                     = True

isPolymorphic (ValueType t) = isPolymorphic' t

assignForType t
    | isPolymorphic t
    = error $ "Can't derive a single-assignment op for a polymorphic type " ++ (show t)
    | t == vnone
    = error $ "Can't assign a void type"
    | t == vstring
    = "sets"
    | t == vagent
    = "seta"
    | t == vnumber
    = "setv"

class Typeable a where
    typeOf :: a -> ValueType

data ConstValue =
    CString  String
  | CInteger Int
  | CFloat   Float
  deriving (Eq, Ord)

instance Show ConstValue where
    show (CString s) = "#<s" ++ s ++ ">"
    show (CInteger i) = "#<i" ++ (show i) ++ ">"
    show (CFloat f) = "#<f" ++ (show f) ++ ">"

instance Typeable ConstValue where
    typeOf (CString  _) = vstring
    typeOf (CInteger _) = vnumber
    typeOf (CFloat   _) = vnumber


data Expression =
    EConst ConstValue
  | EBinaryOp String Expression Expression
  | ELexical String
  | EAssign Expression Expression
  | ECall String [Expression]
  deriving (Eq, Ord, Show)

instance Typeable Expression where
    typeOf (EConst c) = typeOf c
    typeOf (EBinaryOp _ e1 e2) = (typeOf e1) `vand` (typeOf e2)
    typeOf (ELexical _) = vall
    typeOf (EAssign _ e) = typeOf e
    typeOf (ECall _ _) = vall

constInt = EConst . CInteger
constFloat = EConst . CFloat
constString = EConst . CString

data Statement =
    SExpr Expression
    deriving (Eq, Ord, Show)
