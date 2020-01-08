{-# LANGUAGE Rank2Types #-}
module Core where

import Mal

-- builtins

-- arithmetic


binOp :: (forall a. Num a => (a -> a -> MalEval a)) -> MalValue -> MalValue -> MalEval MalValue
binOp op (MalRational r1) (MalRational r2) = MalRational <$> op r1 r2
binOp op (MalFloat r1) (MalFloat r2) = MalFloat <$> op r1 r2
binOp op (MalFloat f) (MalRational r) = MalFloat <$> op f (fraction2float r)
binOp op (MalRational r) (MalFloat f) = MalFloat <$> op (fraction2float r) f
binOp _ a b = throw . MalString $ "incompatible type: " ++ show a ++ "and" ++ show b

addValue :: MalValue -> MalValue -> MalEval MalValue
addValue = binOp (\a b -> return $ a + b)
