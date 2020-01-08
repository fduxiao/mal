import Test.Hspec
import NumberSpec

main :: IO ()
main = hspec $ context "number" testNumber
