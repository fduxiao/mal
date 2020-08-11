module Helper (
    module Helper,
    module Test.Hspec,
    module Lexer,
    newPos
) where

import Test.Hspec
import Lexer
import Text.Parsec.Pos(newPos)

as :: (a -> b) -> a -> b
as = ($)

infixr 0 `as`

shouldParse :: (Show a, Eq a) => Parser a -> String -> a -> Expectation
shouldParse parser content value = case parse parser "test" content of
    Right a -> a `shouldBe` value
    Left err -> expectationFailure (show err)

shouldNotParse :: Parser a -> String -> Expectation
shouldNotParse parser content = case parse parser "test" content of
    Right a -> expectationFailure $ "Parser pased \"" ++ content ++ "\""
    Left err -> return ()
