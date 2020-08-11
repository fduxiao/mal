module Reader (
    module Reader, 
    parse, 
    Parser,
    module Mal,
) where

import Mal
import Lexer
import Data.Char(isDigit)

atPos :: Parser MalAST -> Parser MalAST
atPos p = do
    pos <- getPosition 
    At pos <$> p

value :: Parser MalValue -> Parser MalAST
value p = atPos $ Value <$> p

readNumber :: Parser MalAST
readNumber = value $ try (MalFloat <$> float)
    <|> try (MalRational <$> frac)
    <|> (MalRational . (%1) <$> int)

readString :: Parser MalAST
readString = value $ MalString <$> malStr

readNil :: Parser MalAST
readNil = value $ literal "nil" Nil

readTrue :: Parser MalAST
readTrue = value $ literal "true" (MalBool True)

readFalse :: Parser MalAST
readFalse = value $ literal "false" (MalBool False)

readCall :: Parser MalAST
readCall = atPos $ parens $ Call <$> many readForm

readVar :: Parser MalAST
readVar = atPos $ Var <$> malVar

readValue :: Parser MalAST
readValue = do
    ch <- lookAhead anyChar
    case ch of
        '"' -> readString
        '.' -> readNumber
        '-' -> readNumber
        _ | isDigit ch -> readNumber
        _ -> try readNil <|> try readTrue <|> try readFalse <|> readVar

readForm :: Parser MalAST
readForm = do
    ch <- lookAhead anyChar
    case ch of
        '(' -> readCall 
        _ -> readValue
