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
--import qualified Text.ParserCombinators.Parsec.Pos as Pos
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr

import Control.Monad
import Data.Maybe
import Kaos.AST
--import Debug.Trace
import Kaos.Emit (emitConst)
import Kaos.Core (mergeAccess)
-- TODO: move mergeAccess to AST
import Kaos.Toplevel

typeName :: Parser CAOSType
typeName = (reserved "agent"    >> return typeObj)
        <|>(reserved "numeric"  >> return typeNum)
        <|>(reserved "string"   >> return typeStr)
--        <|>(reserved "any"      >> return typeAny)
        <|>(reserved "void"     >> return typeVoid)

root :: Parser KaosSource
root =  many1 kaosUnit
    <|> simpleScript

simpleScript :: Parser KaosSource
simpleScript = liftM (\x -> [InstallScript x]) bareBlock

kaosUnit :: Parser KaosUnit
kaosUnit =  (installScript <?> "install script")
        <|> (removeScript  <?> "removal script")
        <|> (macroBlock    <?> "macro definition")
        <|> (agentScript   <?> "normal script")
        <|> (ovdecl        <?> "object variable declaration")
        <|> (nullOp)

nullOp :: Parser KaosUnit
nullOp = do
    reservedOp ";"
    return $ InstallScript (SBlock [])

ovdecl :: Parser KaosUnit
ovdecl = do
    reserved "ovar"
    t <- typeName
    name <- identifier
    idx <- option Nothing $ fmap Just idxM
    return $ OVDecl name idx t
    where
        idxM :: Parser Int
        idxM = do
            reservedOp "["
            n <- fmap fromIntegral natural
            reservedOp "]"
            return n


installScript :: Parser KaosUnit
installScript = reserved "install" >> liftM InstallScript (braces bareBlock)

removeScript :: Parser KaosUnit
removeScript  = reserved "remove" >> liftM RemoveScript (braces bareBlock)

macroArg :: Parser MacroArg
macroArg = do
    typ  <- typeName
    name <- identifier
    defaultval <- option Nothing argDefaultNote
    return $ MacroArg name typ defaultval

argDefaultNote :: Parser (Maybe ConstValue)
argDefaultNote = do
    reservedOp "="
    liftM Just constVal

macroType :: Parser MacroType
macroType = (try (symbol "set") >> return MacroLValue)
        <|> (try (symbol "iterator") >> macroIterator)
        <|> (return MacroRValue)
    where
        macroIterator = do
            args <- parens $ commaSep typeName
            return $ MacroIterator args

macroTypePrefix :: MacroType -> String -> String
macroTypePrefix MacroLValue s = "set:" ++ s
macroTypePrefix (MacroIterator _) s = "iter:" ++ s
macroTypePrefix MacroRValue s = s


macroBlock :: Parser KaosUnit
macroBlock = do
    reserved "define"
    mtyp <- macroType
    name <- liftM (macroTypePrefix mtyp) identifier
    args <- parens (commaSep macroArg)
    when (([] /=)
         .filter (isNothing . maDefault)
         .dropWhile (isNothing . maDefault)
         $ args) $ fail "All default macro arguments must be at the end of the argument list"
    retType <- option typeVoid (reserved "returning" >> typeName)
    when (retType /= typeVoid && mtyp /= MacroRValue) $
        fail "Non-rvalue macros must be void"
    code <- braces bareBlock
    return $ MacroBlock $ defaultMacro  { mbName = name
                                        , mbType = mtyp
                                        , mbArgs = args
                                        , mbCode = code
                                        , mbRetType = retType
                                        }

smallNatural :: forall i. (Integral i, Bounded i) => Parser i
smallNatural = try gen <?> desc
    where
        gen = do
            n <- natural
            when (n < minI || n > maxI) $ fail ("Value " ++ (show n) ++ " is out of range")
            let v = (fromInteger n) :: i
            return v
        minN :: i
        minN = minBound
        minI = toInteger minN
        maxN :: i
        maxN = maxBound
        maxI = toInteger maxN
        desc = "integer (" ++ (show minN) ++ ".." ++ (show maxN) ++ ")"

agentScript :: Parser KaosUnit
agentScript = do
    reserved "script"
    symbol "("
    fmly <- smallNatural
    symbol ","
    gnus <- smallNatural
    symbol ","
    spcs <- smallNatural
    symbol ","
    scrp <- smallNatural
    symbol ")"
    code <- braces bareBlock
    return $ AgentScript fmly gnus spcs scrp code
    

whiteSpace :: Parser ()
whiteSpace= P.whiteSpace lexer
lexeme :: Parser String -> Parser String
lexeme    = P.lexeme lexer
symbol :: String -> Parser String
symbol    = P.symbol lexer
natural :: Parser Integer
natural   = P.natural lexer
float   :: Parser Double
float     = P.float lexer
parens :: Parser t -> Parser t
parens    = P.parens lexer
semi :: Parser String
semi      = P.semi lexer
identifier :: Parser String
identifier= P.identifier lexer
reserved :: String -> Parser ()
reserved  = P.reserved lexer
reservedOp :: String -> Parser ()
reservedOp= P.reservedOp lexer
braces :: Parser t -> Parser t
braces    = P.braces lexer

-- Do two things, return the first
(>>>) :: Monad m => m a -> m b -> m a
a >>> b = do { r <- a; b; return r }

manySep :: Parser t -> Parser dummy -> Parser [t]
manySep m sep = (try $ manySep' m sep) <|> return []
manySep' :: Parser t -> Parser dummy -> Parser [t]
manySep' m sep = do
    v <- m
    (try (do {sep; l <- manySep' m sep; return $ v:l}) <|> return [v])

commaSep :: Parser t -> Parser [t]
commaSep m = manySep m $ symbol ","

--maybeParens :: Parser t -> Parser t
--maybeParens p = parens p <|> p

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser 
         (emptyDef
         { reservedOpNames = ["*","/","+","-",
                              ",","&&","||",
                              "|","&",
                              ">","<",">=","<=","!=","==",
                              "!",".","=","[","]",".",
                              "*=", "/=", "+=", "-="]
         , reservedNames   = [
            -- toplevel
            "install", "remove", "script", "define",
            -- types
            "numeric", "string", "agent", "void", "returning",
            -- everything else
            "if", "else", "do", "until", "while", "for", "_caos", "_inst"]
         , caseSensitive   = True
         , commentLine     = "#"
         })

expr :: Parser (Expression String)
expr    = buildExpressionParser table factor
        <?> "expression"

table :: [[Operator Char () (Expression String)]]
table   = [[Prefix (do { reservedOp "!"; return $ EBoolCast . BNot . BExpr})]
          ,[Postfix $ try objCall <|> bareCall]
          ,[op "*" "mulv" AssocLeft, op "/" "divv" AssocLeft]
          ,[op "&" "andv" AssocLeft, op "|" "orrv" AssocLeft]
          ,[op "+" "addv" AssocLeft, op "-" "subv" AssocLeft]
          ,map mkCompar comparOps
          ,[Infix (do { reservedOp "&&"; return $ \a b -> EBoolCast (BAnd (BExpr a) (BExpr b))}) AssocLeft
           ,Infix (do { reservedOp "||"; return $ \a b -> EBoolCast (BOr  (BExpr a) (BExpr b))}) AssocLeft]
          ,[aop "*=" "mulv" AssocRight, aop "/=" "divv" AssocRight,
            aop "-=" "subv" AssocRight, aop "+=" "addv" AssocRight]
          ,[eqop]
          ]          
        where
          objCall = do
            reservedOp "."
            (ECall name args) <- funcCall
            return $ \obj -> ECall name (obj:args)
          bareCall = do
            reservedOp "."
            name <- identifier
            return $ \obj -> ECall name [obj]

          op s f assoc
             = Infix (do{ reservedOp s; return $ EBinaryOp f } <?> "operator") assoc
          aop s f assoc
             = Infix (do{ reservedOp s; return $ \var exp_ -> EAssign var (EBinaryOp f var exp_) } <?> "operator") assoc
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

integerV :: Parser ConstValue
integerV = fmap (CInteger . fromIntegral) $ negWrap natural

floatV :: Parser ConstValue
floatV = fmap CFloat $ negWrap float

negWrap :: Num n => Parser n -> Parser n
negWrap m = do
    neg <- (char '-' >> return negate) <|> return id
    fmap neg m

funcCall :: Parser (Expression String)
funcCall = do
    name <- identifier
    args <- parens $ (try $ commaSep (expr <?> "argument") <|> return [])
    return $ ECall name args
    <?> "function call"

constVal :: Parser ConstValue
constVal = (stringLit <|> try floatV <|> integerV)
    <?> "constant value"

factor :: Parser (Expression String)
factor  =   parens expr
        <|> try funcCall
        <|> liftM EConst constVal
        <|> lexical
        <?> "simple expression"

stringLit :: Parser ConstValue
stringLit = do
        char '"'
        str <- liftM concat $ manyTill stringChar $ char '"'
        whiteSpace
        return $ CString $ "\"" ++ str ++ "\""
    where
        stringChar =
            do { char '\\'; c <- anyChar; return ['\\', c] } <|>
            do { c <- anyChar; return [c] }

lexical :: Parser (Expression String)
lexical = liftM ELexical identifier

exprstmt :: Parser (Statement String)
exprstmt = (do {
            e <- expr;
            symbol ";";
            return $ SExpr e
            })

ifstmt :: Parser (Statement String)
ifstmt = do
    reserved "if"
    cond <- parens $ fmap BExpr expr
    block1 <- fmap SBlock $ braces $ many statement
    block2 <- fmap SBlock $ [] `option`
                            (reserved "else" >> (braces $ many statement))
    return $ SCond cond block1 block2

dostmt :: Parser (Statement String)
dostmt = do
    reserved "do"
    block <- fmap SBlock $ braces $ many statement
    cond <- whileP <|> untilP
    return $ SDoUntil cond block
    where
        untilP = reserved "until" >> fmap BExpr expr
        whileP = reserved "while" >> fmap (BNot . BExpr) expr

whileuntil :: Parser (Statement String)
whileuntil = do
    invert <- ( (reserved "while" >> return BNot) <|> (reserved "until" >> return id ))
    cond <- expr
    block <- fmap SBlock $ braces $ many statement
    return $ SUntil (invert $ BExpr cond) block

forloop :: Parser (Statement String)
forloop = do
    reserved "for"
    symbol "("
    initE <- expr
    semi
    condE <- expr
    semi
    incrE <- expr
    symbol ")"
    codeS <- fmap SBlock $ braces $ many statement
    return $ SBlock [SExpr initE, SUntil (BNot $ BExpr condE) (SBlock [codeS, SExpr incrE])]

instblock :: Parser (Statement String)
instblock = do
    reserved "_inst"
    liftM (SInstBlock . SBlock) $ braces $ many statement

iterCall :: Parser (Statement String)
iterCall = try $ do
    (ECall name args) <- funcCall
    (argnames, block) <- braces inner
    return $ SIterCall name args argnames block
    where
        inner = do
            argNames <- option [] argList
            block <- fmap SBlock $ many statement
            return (argNames, block)
        argList = do
            symbol "|"
            names <- commaSep identifier
            symbol "|"
            return names

declaration :: Parser (Statement String)
declaration = do
    t <- typeName
    decls <- commaSep decl
    return $ SDeclare t decls
    where
        decl :: Parser (String, Maybe (Expression String))
        decl = do
            i <- identifier
            initVal <- option Nothing (reservedOp "=" >> fmap Just expr)
            return (i, initVal)

statement :: Parser (Statement String)
statement = inlineCAOS
        <|> declaration
        <|> iterCall
        <|> exprstmt
        <|> ifstmt
        <|> dostmt
        <|> whileuntil
        <|> instblock
        <|> forloop
        <|> nullStatement
        <?> "statement"

nullStatement :: Parser (Statement String)
nullStatement = do
    semi
    return (SBlock [])

bareBlock :: Parser (Statement String)
bareBlock = do
    s <- many statement
    return $ SBlock s
    <?> "bare script"

parser :: Parser KaosSource
parser = whiteSpace >> root >>> eof


inlineCAOS :: Parser (Statement String)
inlineCAOS = liftM SICaos (reserved "_caos" >> inlineCAOSBlock)
    <?> "inline CAOS"

inlineCAOSBlock :: Parser ([InlineCAOSLine String])
inlineCAOSBlock = (braces $ many (caosStmt >>> semi))
    <?> "inline CAOS block"

caosStmt :: Parser (InlineCAOSLine String)
caosStmt = (caosPragma <|> caosCommand)
    <?> "inline CAOS statement"

caosPragma :: Parser (InlineCAOSLine String)
caosPragma = do
    reservedOp "."
    (caosInlineAssign   <|>
     caosAssign         <|>
     try caosTargZap    <|> 
     caosTarg           <|>
     caosLoop           <|>
     caosKaos)          <?> "inline CAOS directive"

caosTargZap :: Parser (InlineCAOSLine String)
caosTargZap = do
    symbol "targ"
    symbol "zap"
    return ICTargZap

caosInlineAssign :: Parser (InlineCAOSLine String)
caosInlineAssign = do
    try $ symbol "inline"
    symbol "let"
    v1 <- caosVarName
    symbol "="
    repl <- many caosWord
    return $ ICLValue minBound v1 repl

caosKaos :: Parser (InlineCAOSLine String)
caosKaos = do
    symbol "kaos"
    block <- fmap SBlock $ braces (many statement)
    return $ ICKaos block

caosAssign :: Parser (InlineCAOSLine String)
caosAssign = do
        try $ symbol "let"
        v1 <- caosVarName
        symbol "="
        try (finishConst v1) <|> (finishVar v1)
    where
        finishVar v1 = do
            v2 <- caosVarName
            return $ ICAssign v1 v2 
        finishConst v1 = do
            v2 <- constVal
            return $ ICConst v1 v2

caosLoop :: Parser (InlineCAOSLine String)
caosLoop = do
    symbol "loop"
    body <- inlineCAOSBlock
    return $ ICLoop body

caosTarg :: Parser (InlineCAOSLine String)
caosTarg = do
    symbol "targ"
    dir <- lexeme $ liftM (:"") (oneOf "<>")
    let op = mapOp dir
    v <- caosVarName
    body <- inlineCAOSBlock 
    return $ op v body
    where
        mapOp ">" = ICTargWriter
        mapOp "<" = ICTargReader
        mapOp  _  = undefined

caosCommand :: Parser (InlineCAOSLine String)
caosCommand = liftM ICLine $ many caosWord

caosWord :: Parser (InlineCAOSToken String)
caosWord = caosVarRef <|> try caosConstLiteral <|> caosWordLiteral

caosVarRef :: Parser (InlineCAOSToken String)
caosVarRef = do
    n <- caosVarName
    ac <- caosVarAccess
    whiteSpace
    return $ ICVar n ac

caosVarName :: Parser String
caosVarName = char '$' >> identifier

caosVarAccess :: Parser (AccessType)
caosVarAccess  = caosVarAccess' <?> "access mode"
caosVarAccess' :: Parser (AccessType)
caosVarAccess' = parens $ do
    modeflags <- many (oneOf "rwm" >>> whiteSpace)
    return $ foldl mergeAccess NoAccess (map accessType modeflags)
    where
        accessType 'r' = ReadAccess
        accessType 'w' = WriteAccess
        accessType 'm' = MutateAccess
        accessType v   = error $ "Impossible: Access mode '" ++ [v] ++ "' got through oneOf"

caosWordLiteral :: Parser (InlineCAOSToken String)
caosWordLiteral = do
    lead <- letter <|> oneOf "_"
    remain <- many (alphaNum <|> oneOf "$#:?!_+-")
    whiteSpace
    return $ ICWord (lead:remain)

caosConstLiteral :: Parser (InlineCAOSToken String)
caosConstLiteral = liftM (ICWord . emitConst) $ constVal
