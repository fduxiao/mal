module Lexer (
    module Lexer, 
    module Text.Parsec, 
    (%), 
    Parser
) where

import Control.Monad(when, void)
import Data.Ratio

import Text.Parsec
import Text.Parsec.Char (digit, char, anyChar)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok
import Mal

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style 
    where
        style = emptyDef {
                Tok.commentLine = ";", 
                Tok.reservedOpNames = ["~", "@", "~@", "`", "'"], 
                Tok.reservedNames = ["true", "false", "nil"]
            }

-- numbers
neg :: Num a => Parser a -> Parser a
neg p = do
    char '-'
    a <- p
    return $ -1 * a

signed :: Num a => Parser a -> Parser a
signed p = try (neg p) <|> p

float :: Parser Double
float = signed $ do
    i <- many digit
    p <- char '.'
    d <- many digit
    case i ++ p:d of
        "." -> unexpected "."
        a | d == "" -> return . read $ a ++ "0"
        a | i == "" -> return . read $ '0':a
        a -> return $ read a

int :: Parser Integer
int = signed $ Tok.integer lexer

frac :: Parser Fraction
frac = signed $ do
    n <- many1 digit
    char '/'
    d <- many1 digit
    return $ read n % read d


parens :: Parser a -> Parser a
parens a = try (Tok.parens lexer a) <|> Tok.braces lexer a

malStr :: Parser String
malStr = Tok.stringLiteral lexer

malVar :: Parser String
malVar = try (Tok.identifier lexer) <|> Tok.operator lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer


literal :: String -> a -> Parser a
literal name value = reserved name >> return value
