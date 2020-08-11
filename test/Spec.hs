import Test.Hspec

import NumberSpec
import ParserSpec

main :: IO ()
main = do
    hspec $ context "number" testNumber
    hspec $ context "parser" testParser
