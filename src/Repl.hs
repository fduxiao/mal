module Repl(
    repl, 
    liftIO
) where

import Reader
import Mal
import Control.Monad.IO.Class

repl :: String -> IO ()
repl line =  putStrLn $ case parse readForm "stdin" line of
    Right r -> show r
    Left e -> show e
