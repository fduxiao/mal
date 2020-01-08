module Env where

import Data.Map as Map

type Defn a = Map.Map String a

data Env a = Env {
    callStack :: [String],
    defn :: [Defn a]
}
