module NumberSpec where

import Helper
import Lexer

testFloat :: Spec
testFloat = describe "Parse float number" $ do
    it "fails on an `integer`" $
        float `shouldNotParse` "1"
    it "fails on a `point`" $
        float `shouldNotParse` "."
    it "returns a normal float" $
        float `shouldParse` "1.0" `as` 1
    it "returns a negative float" $
        float `shouldParse` "-1.0" `as` -1
    it "returns a float without decimals" $
        float `shouldParse` "1." `as` 1
    it "returns a float with only decimals" $
        float `shouldParse` ".1" `as` 0.1
    it "returns a negative float with only decimals" $
        float `shouldParse` "-.123" `as` -0.123


testInt :: Spec
testInt = describe "Parse integer number" $ do
    it "takes integral part of a float" $
        int `shouldParse` "-1." `as` -1
    it "returns a normal int" $
        int `shouldParse` "01" `as` 1
    it "returns a negative int" $
        int `shouldParse` "-1" `as` -1

testFrac :: Spec
testFrac = describe "Parse fractional number" $ do
    it "fails on an `integer`" $
        frac `shouldNotParse` "-1"
    it "fails on a strange fractional" $
        frac `shouldNotParse` "-1/"
    it "fails on a strange fractional" $
        frac `shouldNotParse` "/2"
    it "returns a normal frac" $
        frac `shouldParse` "1/2" `as` 1 % 2
    it "returns a negative frac" $
        frac `shouldParse` "-1/3" `as` -1 % 3

testNumber :: Spec
testNumber = do
    testInt 
    testFloat
    testFrac
