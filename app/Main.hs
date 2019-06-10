module Main where

import Lib (compile)
import Parser (myParse)
import System.IO (writeFile)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    input <- getContents
    case myParse "(stdin)" input of
        Left err -> do
            putStrLn "failed to parse"
            print err
        Right tree -> do
            case compile tree of
                Left err -> putStrLn "failed to compile" >> putStrLn err
                Right result -> putStr result

-- main = putStrLn "please what is going on"