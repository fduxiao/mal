module Mal (
    module Mal, 
    SourcePos, 
    throw,
    (%)
) where

import Data.Ratio
import Data.Map as Map
import Text.Parsec.Pos

import Eval
import Env

type Fraction = Ratio Integer

fraction2float :: Fraction -> Double
fraction2float a = fromInteger (numerator a) / fromInteger (denominator a)

joinList :: (Show a) => [a] -> String
joinList [] = ""
joinList [x] = show x
joinList (x:xs) = show x ++ " " ++ joinList xs

type MalEval = Eval (Env MalValue) MalValue
data MalParams = MalParams {
    position :: [String],
    arbitrary :: Maybe String
} 

instance Show MalParams where
    show (MalParams position arbitrary) = "(" ++ joinList position ++ maybe "" (" &" ++) arbitrary ++ ")"

data MalValue = Nil 
    | MalRational Fraction 
    | MalBool Bool
    | MalFloat Double 
    | MalString String
    | MalList [MalValue]
    -- MalFunc name ast body (ast is used for tail recursion later)
    | MalFunc {
        name :: Maybe String,
        params :: [String],
        ast :: Maybe MalAST, 
        body :: [MalValue] -> MalEval MalValue
    }

instance Show MalValue where
    show Nil = "nil"
    show (MalRational r) = show (numerator r) ++ case denominator r of
        1 -> ""
        a -> "/" ++ show a 
    show (MalBool b) = show b
    show (MalFloat f) = show f
    show (MalString s) = show s  -- while a string should be "content"
    show (MalList xs) = "(" ++ joinList xs ++ ")"
    show (MalFunc (Just name) _ Nothing  _) = "#<bulitin:" ++ name ++ ">"
    show (MalFunc Nothing _ Nothing  _) = "#<bulitin>"
    show (MalFunc (Just name) params (Just ast)  _) = "(def!" ++ name ++ "(fn* " ++ show params ++ show ast ++ ")" ++ ")"
    show (MalFunc Nothing params (Just ast)  _) = "(fn* " ++ show params ++ show ast ++ ")"

data MalAST = Value SourcePos MalValue | Var SourcePos String | Call SourcePos [MalAST]

instance Show MalAST where
    show (Value _ v) = show v 
    show (Var _ s) = s  -- a symbol is just represented in REPL as the name
    show (Call _ args) = "(" ++ joinList args ++ ")"
