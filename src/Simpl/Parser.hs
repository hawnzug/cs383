{-# LANGUAGE OverloadedStrings #-}
module Simpl.Parser where

import Control.Applicative ((<|>))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

import Unbound.Generics.LocallyNameless as Unbound

import Simpl.Core

_func f c = do
  symbol f
  n <- identifier
  symbol "=>"
  e <- expr
  let name = Unbound.s2n n
  return $ c (Unbound.bind name e)

_cond = do
  symbol "if" 
  c <- expr
  symbol "then"
  e1 <- expr
  symbol "else"
  e2 <- expr
  return (Cond c e1 e2)

_let = do
  symbol "let"
  n <- identifier
  symbol "="
  e1 <- expr
  symbol "in"
  e2 <- expr
  symbol "end"
  let name = Unbound.s2n n
      pattern = (name, Unbound.embed e1)
  return (Let (Unbound.bind pattern e2))

_loop = do
  symbol "while"
  e1 <- expr
  symbol "do"
  e2 <- expr
  return (Loop e1 e2)

_pair = do
  symbol "("
  e1 <- expr
  symbol ","
  e2 <- expr
  symbol ")"
  return (Pair e1 e2)


expr :: Parser Expr
expr = makeExprParser expr1 table <?> "expression"

expr1 = (choice
  [ _cond <?> "conditional statement"
  , _loop <?> "while loop"
  , _let <?> "let statement"
  , _func "fn" Fn <?> "function"
  , _func "rec" Rec <?> "recursive function"
  , term
  ]) <?> "expr"

term :: Parser Expr
term = do
  t <- aexpr
  ts <- many aexpr
  return $ if null ts
    then t
    else (foldl1 App (t:ts))

aexpr :: Parser Expr
aexpr = (choice
  [ try _pair
  , try (Unit <$ symbol "()")
  , parens expr
  , (Var . Unbound.s2n) <$> identifier
  , IntLit <$> integer
  , BoolLit True <$ symbol "true"
  , BoolLit False <$ symbol "false"
  , Nil <$ symbol "nil"
  , Ref <$> (symbol "ref" *> aexpr)
  , Deref <$> (symbol "!" *> aexpr)
  , Not <$> (symbol "not" *> aexpr)
  , Neg <$> (symbol "~" *> aexpr)
  ]) <?> "simple expression"

type Parser = Parsec Void String

sc = Lex.space space1 empty (Lex.skipBlockCommentNested "(*" "*)")
lexeme = Lex.lexeme sc
integer = lexeme Lex.decimal
symbol = Lex.symbol sc
parens = between (symbol "(") (symbol ")")

table =
  [ [ binaryL "*" Mult , binaryL "/" Div , binaryL "%" Mod ]
  , [ binaryL "+" Add , binaryL "-" Sub ]
  , [ binaryR "::" Cons ]
  , [ binary "="  Eq
    , binary "<>" Neq
    , binary "<=" LessEq
    , binary "<"  Less
    , binary ">=" GreaterEq
    , binary ">"  Greater
    ]
  , [ binaryR "andalso" AndAlso ]
  , [ binaryR "orelse" OrElse ]
  , [ binary ":=" Assign ]
  , [ binaryL ";" Seq ]
  ]

binary  name f = InfixN (f <$ symbol name)
binaryL name f = InfixL (f <$ symbol name)
binaryR name f = InfixR (f <$ symbol name)

reservedWords =
  [ "andalso", "orelse" , "true", "false", "nil", "ref", "do", "not"
  , "fn", "let", "in", "end", "if", "then", "else", "while"
  ]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> (char '_' <|> lowerChar)
            <*> many (char '_' <|> char '\'' <|> alphaNumChar)
    check x =
      if x `elem` reservedWords
      then fail $ "keyword " ++ show x ++ " cannot be an identifier"
      else return x

prog = sc *> expr <* eof
parseProg input filename =
  case parse prog filename input of
    Left err -> Left $ parseErrorPretty err
    Right e -> Right e 
parseMb = parseMaybe prog
