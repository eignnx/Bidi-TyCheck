-- Based on https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter3/calc/Parser.hs
module Parse (
  parseExpr
) where

import Syntax
import Ty

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "/-"
  , Tok.commentEnd      = "-/"
  , Tok.commentLine     = "//"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = ["def", "true", "false", "let", "in", "if", "then", "else"]
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

natural :: Parser Integer
natural = Tok.natural lexer

identifier :: Parser String
identifier = Tok.identifier lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

colon :: Parser String
colon = Tok.colon lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

table :: Ex.OperatorTable String () Identity Expr
table = [
    [
      Ex.Postfix (do { colon; t <- ty; return $ \e -> Ann e t })
    ]
  ]


-- if/then/else
ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (If cond tr fl)

-- Constants
unit, true, false, nat, lambda :: Parser Expr
unit  = reservedOp "()"  >> return UnitConst
true  = reserved "true"  >> return (BoolConst True)
false = reserved "false" >> return (BoolConst False)
nat = NatConst <$> natural

listTyP :: Parser Ty
listTyP = brackets $ do
  t <- ty
  return $ listTy t

setTyP :: Parser Ty
setTyP = do
  symbol "#"
  symbol "["
  t <- ty
  symbol "]"
  return $ setTy t

mapTyP :: Parser Ty
mapTyP = do
  symbol "#"
  symbol "["
  k <- ty
  colon
  v <- ty
  symbol "]"
  return $ mapTy k v

optionTyP :: Parser Ty
optionTyP = do
  symbol "?"
  t <- ty
  return $ optionTy t

tyTable :: Ex.OperatorTable String () Identity Ty
tyTable = [
    [
      Ex.Infix (do { symbol "!"; return resultTy }) Ex.AssocLeft
    ]
  ]

ty :: Parser Ty
ty = Ex.buildExpressionParser tyTable tyFactor

-- resultTyP :: Parser Ty
-- resultTyP = (ty <* symbol "!") `chainl1` (do { return resultTy })
-- resultTyP = do
--   t <- ty -- Yikes! This Programmer Thinks He Can Get Away With Left Recursion!
--   symbol "!"
--   e <- ty
--   return $ resultTy t e

tyFactor :: Parser Ty
tyFactor
  =   (symbol "()" *> return unitTy)
  <|> listTyP
  <|> setTyP
  <|> mapTyP
  <|> optionTyP
  <|> (try $ do { ident <- identifier; return $ PolyTy ident [] })
  -- <|> try resultTyP

param :: Parser Param
param = try annotated <|> inferred
  where
    annotated = do
      ident <- identifier
      colon
      t <- ty
      return $ AnnParam ident t
    inferred = Infer <$> identifier

lambda = braces $ do
  symbol "|"
  p <- param
  symbol "|"
  body <- expr
  return $ FnExpr p body

letExpr = do
  reserved "let"
  var <- param
  symbol "="
  binding <- expr
  reserved "in"
  body <- expr
  return $ Let var binding body

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor = unit
     <|> true
     <|> false
     <|> nat
     <|> ifthen
     <|> lambda
     <|> letExpr
     <|> parens expr
     <|> (Var <$> identifier)

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = semiSep expr

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s