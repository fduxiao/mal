module ParserSpec where

import Helper
import Reader

testValue :: Spec
testValue = describe "Test Value" $ do
    it "parses number" $
        readValue `shouldParse` "123" `as` At (newPos "test" 1 1) (Value (MalRational (123 % 1)))

    it "parses rational" $
        readValue `shouldParse` "123/4" `as` At (newPos "test" 1 1) (Value (MalRational (123 % 4)))

    it "parses nil" $
        readValue `shouldParse` "nil" `as` At (newPos "test" 1 1) (Value Nil)

    it "parses true" $
        readValue `shouldParse` "true" `as` At (newPos "test" 1 1) (Value (MalBool True))

    it "parses false" $
        readValue `shouldParse` "false" `as` At (newPos "test" 1 1) (Value (MalBool False))

    it "parses rational" $
        readValue `shouldParse` "123/4" `as` At (newPos "test" 1 1) (Value (MalRational (123 % 4)))

    it "parses string" $
        readValue `shouldParse` "\"abc\"" `as` At (newPos "test" 1 1) (Value $ MalString "abc")

    it "doesnot parse call" $
        readValue `shouldNotParse` "(+ 1 2)"

    it "parses variable" $
        readValue `shouldParse` "abc" `as` At (newPos "test" 1 1) (Var "abc")

testForm :: Spec
testForm = describe "Test Form" $ do
    it "parses value" $
        readForm `shouldParse` "+" `as` At (newPos "test" 1 1) (Var "+")

    it "parses form" $
        readForm `shouldParse` "(+ true true2)" `as` At (newPos "test" 1 1) (Call [
            At (newPos "test" 1 2) (Var "+"),
            At (newPos "test" 1 4) (Value (MalBool True)),
            At (newPos "test" 1 9) (Var "true2")
        ])

testParser :: Spec
testParser = do
    testValue
    testForm
