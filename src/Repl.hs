module Repl
    ( repl
    , liftIO
    ) where

import Reader
import Eval
import Mal
import Control.Monad.IO.Class

repl :: String -> IO ()
repl = putStrLn . readForm . eval
