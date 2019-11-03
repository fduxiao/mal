module Lexer where

import Control.Applicative

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style 
    where
        style = emptyDef {
                Tok.commentLine = ";"
            }

integer :: Parser Integer
integer = Tok.integer lexer

fraction :: Parser (Integer, Integer)
fraction = do
    numerator <- integer
    denominator <- integer
    return (numerator, denominator)

float :: Parser Double
float = Tok.float lexer

parens :: Parser a -> Parser a
parens a = Tok.parens lexer a <|> Tok.braces lexer a

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer
