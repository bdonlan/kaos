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

module Kaos.Parser ( parser ) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Pos as Pos
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Monad (liftM, when)
import Kaos.AST
import Debug.Trace

import qualified IO

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
float     = P.float lexer
parens    = P.parens lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer
braces    = P.braces lexer

-- Do two things, return the first
(>>>) :: Monad m => m a -> m b -> m a
a >>> b = do { r <- a; b; return r }

manySep m sep = do
    v <- m
    (try (do {sep; l <- manySep m sep; return $ v:l}) <|> return [v])

commaSep m = manySep m $ symbol ","

maybeParens p = parens p <|> p

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser 
         (emptyDef
         { reservedOpNames = ["*","/","+","-",
                              ",",
                              ">","<",">=","<=","!=","==",
                              "!",".","=","[","]",".",
                              "*=", "/=", "+=", "-="]
         , reservedNames   = ["install","number","if","else","elsif",
                              "do", "until", "while", "forever", "enum",
                              "esee", "etch", "remove", "agent", "script",
                              "string", "atomic", "lock", "inst", "object",
                              "define"]
         , caseSensitive   = True
         , commentLine     = "#"
         })

expr    = buildExpressionParser table factor
        <?> "expression"

table   = [[Prefix (do { reservedOp "!"; return $ EBoolCast . BNot . BExpr})]
          ,[op "*" "mulv" AssocLeft, op "/" "divv" AssocLeft]
          ,[op "+" "addv" AssocLeft, op "-" "subv" AssocLeft]
          ,map mkCompar comparOps
          ,[Infix (do { reservedOp "&&"; return $ \a b -> EBoolCast (BAnd (BExpr a) (BExpr b))}) AssocLeft
           ,Infix (do { reservedOp "||"; return $ \a b -> EBoolCast (BOr  (BExpr a) (BExpr b))}) AssocLeft]
          ,[aop "*=" "mulv" AssocRight, aop "/=" "divv" AssocRight,
            aop "-=" "subv" AssocRight, aop "+=" "addv" AssocRight]
          ,[eqop]
          ]          
        where
          op s f assoc
             = Infix (do{ reservedOp s; return $ EBinaryOp f } <?> "operator") assoc
          aop s f assoc
             = Infix (do{ reservedOp s; return $ \var exp -> EAssign var (EBinaryOp f var exp) } <?> "operator") assoc
          eqop
             = Infix (do{ reservedOp "="; return $ EAssign } <?> "operator") AssocRight
          mkCompar (cstr, ctype) = Infix matcher AssocNone
            where matcher = do
                    reservedOp cstr
                    return $ \a b -> EBoolCast $ BCompare ctype a b
          comparOps = [ ("<" , CLT), (">" , CGT)
                      , ("<=", CLE), (">=", CGE)
                      , ("==", CEQ), ("!=", CNE), ("/=", CNE)
                      ]

integerV = fmap (constInt . fromIntegral) $ negWrap natural

floatV = fmap constFloat $ negWrap float

negWrap :: Num n => Parser n -> Parser n
negWrap m = do
    neg <- (char '-' >> return negate) <|> return id
    fmap neg m

funcCall = do
    name <- identifier
    args <- parens $ (try $ commaSep (expr <?> "argument") <|> return [])
    return $ ECall name args
    <?> "function call"

factor  =   parens expr
        <|> try funcCall
        <|> stringLit
        <|> try floatV
        <|> integerV
        <|> lexical
        <?> "simple expression"

stringLit = do
        char '"'
        str <- liftM concat $ manyTill stringChar $ char '"'
        whiteSpace
        return $ constString $ "\"" ++ str ++ "\""
    where
        stringChar =
            do { char '\\'; c <- anyChar; return ['\\', c] } <|>
            do { c <- anyChar; return [c] }

lexical = liftM ELexical identifier

exprstmt = (do {
            e <- expr;
            symbol ";";
            return $ SExpr e
            })

ifstmt = do
    reserved "if"
    cond <- parens $ fmap BExpr expr
    block1 <- fmap SBlock $ braces $ many statement
    block2 <- fmap SBlock $ [] `option`
                            (reserved "else" >> (braces $ many statement))
    return $ SCond cond block1 block2

dostmt = do
    reserved "do"
    block <- fmap SBlock $ braces $ many statement
    cond <- while <|> until
    return $ SDoUntil cond block
    where
        until = reserved "until" >> fmap BExpr expr
        while = reserved "while" >> fmap (BNot . BExpr) expr

statement = exprstmt
        <|> ifstmt
        <|> dostmt
        <?> "statement"
root = simpleScript

simpleScript = do
    s <- many statement
    return $ SBlock s
    <?> "bare script"

parser :: Parser (Statement String)
parser = whiteSpace >> root >>> eof
