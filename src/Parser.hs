{-# LANGUAGE Haskell2010, FlexibleContexts, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}

module Parser (
  parseSExp,
  parseRule,
  parseSExps,
  parseRules,
  )
where

-- TODO: introduce precedence & try

import Data.Void
import Text.Parsec
import qualified Text.Parsec.Token as P
import Syntax
import Rewriting
import Control.Applicative (liftA2)

-- Additional parser state
type ParserState = ()

type Parser = Parsec String ParserState

-- Definition for tokens
languageDef = P.LanguageDef {
  -- Not fully SMT-LIB2 compliant comments
  P.commentStart = "(comment",
  P.commentEnd = ")",
  -- s-exp line comments
  P.commentLine = ";",
  -- Allow nested comments
  P.nestedComments = True,
  P.identStart = atomLetter,
  P.identLetter = atomLetter,
  P.opStart = oneOf "",
  P.opLetter = oneOf "",
  P.reservedNames = [],
  P.reservedOpNames = [],
  P.caseSensitive = True
  }
  where atomLetter = alphaNum <|> oneOf "_'+-*/<=>!@#$%^&~`|?:"

-- a helper that fails the parser if given predicate is false on the parser's
-- return data
parser <??> pred = parser >>= \a -> if pred a then return a else parserZero
infixl 6 <??>

lexer = P.makeTokenParser languageDef

inparen = P.parens lexer

atom :: Parser (SExp a)
atom = try $ Atom <$> P.identifier lexer <??> ((/= '$') . head)
hole :: Parser (SExp HoleVar)
hole = try $ do
  ('$' : tailS) <- P.identifier lexer
  return $ Hole $ HoleVar $ read tailS

sexp :: Parser (SExp Void)
sexp = atom <|> inparen (App <$> many sexp)

-- left-hand or right-hand side of a rule
ruleSide :: Parser (SExp HoleVar)
ruleSide = hole <|> atom <|> inparen (App <$> many ruleSide)

rule :: Parser Rule
rule = liftA2 Rule ruleSide (P.reserved lexer "->" *> ruleSide)

parseAll p = runParser (P.whiteSpace lexer *> p <* eof) ()

parseSExp = parseAll sexp
parseSExps = parseAll (many sexp)
parseRule = parseAll rule
parseRules = parseAll (many rule)
