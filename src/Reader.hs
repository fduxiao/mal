module Reader (
    module Reader, 
    parse, 
    Parser
) where

import Mal
import Lexer
import Data.Char(isDigit)

value :: Parser MalValue -> Parser MalAST
value p = do
    pos <- getPosition
    Value pos <$> p

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
readCall = do
    pos <- getPosition 
    parens $ Call pos <$> many readForm

readVar :: Parser MalAST
readVar = do
    pos <- getPosition
    Var pos <$> malVar

readValue :: Parser MalAST
readValue = do
    pos <- getPosition
    ch <- lookAhead anyChar
    case ch of
        '"' -> readString
        '.' -> readNumber
        '-' -> readNumber
        _ | isDigit ch -> readNumber
        _ -> readVar

readForm :: Parser MalAST
readForm = do
    ch <- lookAhead anyChar
    case ch of
        '(' -> readCall 
        _ -> readValue
