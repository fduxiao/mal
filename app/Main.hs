module Main where

import System.Console.Haskeline
import Repl

main :: IO ()
main = runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "user> "
            case minput of
                Nothing -> return ()
                Just input -> do 
                    liftIO $ repl input
                    loop
