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
            ConstValue(..), Expression(..), Statement(..),
            constInt, constFloat, constString,
            ) where

import Utils

data ConstValue =
    CString  String
  | CInteger Int
  | CFloat   Double
  deriving (Eq, Ord)

instance Show ConstValue where
    show (CString s) = "#<s" ++ s ++ ">"
    show (CInteger i) = "#<i" ++ (show i) ++ ">"
    show (CFloat f) = "#<f" ++ (show f) ++ ">"

data Expression l =
    EConst ConstValue
  | EBinaryOp String (Expression l) (Expression l)
  | ELexical l
  | EAssign (Expression l) (Expression l)
  | ECall String [Expression l]
  deriving (Eq, Ord, Show)

constInt = EConst . CInteger
constFloat = EConst . CFloat
constString = EConst . CString

data Statement l =
    SExpr  (Expression l)
  | SBlock [Statement l] 
    deriving (Eq, Ord, Show)
