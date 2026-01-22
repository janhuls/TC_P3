{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}

-- This module implements a parser for
--  (a subset of) the C# programming language

module CSharp.Parser where

import CSharp.AbstractSyntax

import ParseLib.Derived hiding (braced, bracketed, parenthesised)
import Control.Applicative
import Data.Char
import Data.Maybe
import GHC.Read (paren)

---- Begin Pretty-printing functions for C# syntax ----

printType :: Type -> String
printType t = case t of
  TyInt -> "int"
  TyBool -> "bool"

printOperator :: Operator -> String
printOperator op = case op of
  OpAdd -> "+"
  OpSub -> "-"
  OpMul -> "*"
  OpDiv -> "/"
  OpMod -> "%"
  OpAnd -> "&&"
  OpOr -> "||"
  OpXor -> "^"
  OpLeq -> "<="
  OpLt -> "<"
  OpGeq -> ">="
  OpGt -> ">"
  OpEq  -> "=="
  OpNeq -> "!="
  OpAsg -> "="

printKeyword :: Keyword -> String
printKeyword k = case k of
  KeyIf    -> "if"
  KeyElse  -> "else"
  KeyWhile -> "while"
  KeyReturn -> "return"
  KeyTry   -> "try"
  KeyCatch -> "catch"
  KeyClass -> "class"
  KeyVoid  -> "void"
  KeyFor   -> "for"

printPunctuation :: Punctuation -> String
printPunctuation p = case p of
  POpen     -> "("
  PClose    -> ")"
  SOpen     -> "["
  SClose    -> "]"
  COpen     -> "{"
  CClose    -> "}"
  Comma     -> ","
  Semicolon -> ";"

printBool :: Bool -> String
printBool b = case b of
  True -> "true"
  False -> "false"

---- End Pretty-printing functions for C# syntax ----

data Keyword
  = KeyIf    | KeyElse
  | KeyWhile | KeyReturn
  | KeyTry   | KeyCatch
  | KeyClass | KeyVoid
  | KeyFor
  deriving (Eq, Show, Ord, Enum, Bounded)

data Punctuation
  = POpen    | PClose
  | SOpen    | SClose
  | COpen    | CClose
  | Comma    | Semicolon
  deriving (Eq, Show, Ord, Enum, Bounded)

----- Begin Lexer -----

data Token
  = Punctuation Punctuation
  | Keyword     Keyword
  | Type        Type
  | Operator    Operator
  | UpperId     Ident
  | LowerId     Ident
  | BoolLit     Bool
  | IntLit      Int
  deriving (Eq, Show, Ord)

lexicalScanner :: Parser Char [Token]
lexicalScanner = lexWhiteSpace *> greedyMaybe (lexToken <* lexWhiteSpace) <* eof

lexToken :: Parser Char (Maybe Token)
lexToken = greedyChoice
  [ lexComment
  , Just . Keyword     <$> lexEnum printKeyword
  , Just . Punctuation <$> lexEnum printPunctuation
  , Just . Type        <$> lexEnum printType
  , Just . Operator    <$> lexEnum printOperator
  , Just . BoolLit     <$> lexEnum printBool
  , Just . IntLit      <$> lexInt
  , Just <$> lexLowerId
  , Just <$> lexUpperId
  ]

lexEnum :: (Bounded a, Enum a, Eq s) => (a -> [s]) -> Parser s a
lexEnum print = choice $ map (\a -> a <$ token (print a)) [minBound..maxBound]

lexInt :: Parser Char Int
lexInt = read <$> greedy1 (satisfy isDigit)

lexLowerId, lexUpperId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> lexIdent
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> lexIdent

lexIdent :: Parser Char Ident
lexIdent = greedy (satisfy isAlphaNum)

lexWhiteSpace :: Parser Char String
lexWhiteSpace = greedy (satisfy isSpace)

lexComment :: Parser Char (Maybe Token)
lexComment = token "//" *> greedy (satisfy (/= '\n')) *> succeed Nothing

greedyMaybe :: Parser s (Maybe a) -> Parser s [a]
greedyMaybe p = getJust <$> greedy p where
  getJust [] = []
  getJust (Just a : xs) = a : getJust xs
  getJust (Nothing : xs) = getJust xs

greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty

----- End Lexer -----

----- Begin Lexer-Parser glue -----

keyword :: Keyword -> Parser Token ()
keyword k = () <$ satisfy (== Keyword k)

punctuation :: Punctuation -> Parser Token ()
punctuation p = () <$ satisfy (== Punctuation p)

sSemi :: Parser Token ()
sSemi = punctuation Semicolon

sIntLit :: Parser Token Int
sIntLit = do
  tok <- anySymbol
  case tok of
    IntLit x -> pure x
    _ -> failp

sBoolLit :: Parser Token Bool
sBoolLit = do
  tok <- anySymbol
  case tok of
    BoolLit x -> pure x
    _ -> failp

sType :: Parser Token Type
sType = do
  tok <- anySymbol
  case tok of
    Type x -> pure x
    _ -> failp

sUpperId :: Parser Token Ident
sUpperId = do
  tok <- anySymbol
  case tok of
    UpperId x -> pure x
    _ -> failp

sLowerId :: Parser Token Ident
sLowerId = do
  tok <- anySymbol
  case tok of
    LowerId x -> pure x
    _ -> failp

sOperator :: Parser Token Operator
sOperator = do
  tok <- anySymbol
  case tok of
    Operator x -> pure x
    _ -> failp

----- End Lexer-Parser glue -----

----- Begin Parser ----

pClass :: Parser Token Class
pClass = Class <$ keyword KeyClass <*> sUpperId <*> braced (many pMember)

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> MemberM <$> pRetType <*> sLowerId <*> methArgList <*> pBlock
  where
    methArgList = parenthesised (option (listOf pDecl (punctuation Comma)) [])

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

pStatDecl :: Parser Token Stat
pStatDecl = pStat <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr <* sSemi
     <|> StatIf     <$ keyword KeyIf     <*> parenthesised pExpr <*> pStat <*> optionalElse
     <|> StatWhile  <$ keyword KeyWhile  <*> parenthesised pExpr <*> pStat
     <|> StatReturn <$ keyword KeyReturn <*> pExpr <* sSemi
     <|> StatFor    <$ keyword KeyFor    <*> pForHeader <*> pStat
     <|> pBlock
  where
    optionalElse = option (keyword KeyElse *> pStat) (StatBlock [])
    pExprDecl = ForExpr <$> pExpr <|> ForDecl <$> pDecl
    pExprDecls = option (listOf pExprDecl (punctuation Comma)) []
    pForHeader = parenthesised $
      (,,)
        <$> pExprDecls <* sSemi
        <*> pExpr      <* sSemi
        <*> pExprDecls

pLiteral :: Parser Token Literal
pLiteral = LitBool <$> sBoolLit <|> LitInt <$> sIntLit

pExprSimple :: Parser Token Expr
pExprSimple =
      ExprLit  <$> pLiteral
  <|> ExprCall <$> sLowerId <*> pExprList
  <|> ExprVar  <$> sLowerId
  <|> parenthesised pExpr
  where
    pExprList = parenthesised $ option (listOf pExpr (punctuation Comma)) []

opFrom :: [Operator] -> Parser Token Operator
opFrom ops = do
  tok <- anySymbol
  case tok of
    Operator op | op `elem` ops -> pure op
    _ -> empty

pExprMul = chainl pExprSimple (ExprOper <$> opFrom [OpMul, OpDiv, OpMod])
pExprAdd = chainl pExprMul    (ExprOper <$> opFrom [OpAdd, OpSub])
pExprRel = chainl pExprAdd    (ExprOper <$> opFrom [OpLt, OpLeq, OpGt, OpGeq])
pExprEq  = chainl pExprRel    (ExprOper <$> opFrom [OpEq, OpNeq])
pExprXor = chainl pExprEq     (ExprOper <$> opFrom [OpXor])
pExprAnd = chainl pExprXor    (ExprOper <$> opFrom [OpAnd])
pExprOr  = chainl pExprAnd    (ExprOper <$> opFrom [OpOr])

asgOp :: Parser Token Operator
asgOp = do
  tok <- anySymbol
  case tok of
    Operator OpAsg -> pure OpAsg
    _ -> empty

pExprAssign =
      tryAssign
  <|> pExprOr
  where
    tryAssign = do
      lhs <- pExprOr
      op  <- asgOp
      rhs <- pExprAssign
      case lhs of
        ExprVar _ -> pure (ExprOper op lhs rhs)
        _         -> empty

pExpr = pExprAssign

pDecl = Decl <$> pRetType <*> sLowerId
pDeclSemi = pDecl <* sSemi

pType = sType

pRetType = NV <$> pType <|> TyVoid <$ keyword KeyVoid

parenthesised p = pack (punctuation POpen) p (punctuation PClose)
bracketed     p = pack (punctuation SOpen) p (punctuation SClose)
braced        p = pack (punctuation COpen) p (punctuation CClose)

--- End Parser ----
