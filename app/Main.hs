module Main where

import Lib (compile)
import Parser (myParse)
import System.IO (writeFile)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Data.List (last, intercalate)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            let splitPath = splitOn "/" filePath
                pathBefore = init splitPath
                fileName = head $ splitOn "." (last splitPath)
                writePath = intercalate "/" (pathBefore ++ [fileName ++ ".hack"])
            input <- readFile filePath
            case myParse "(stdin)" input of
                Left err -> do
                    putStrLn "failed to parse"
                    print err
                Right tree -> do
                    case compile tree of
                        Left err -> putStrLn "failed to compile" >> putStrLn err
                        Right result -> writeFile writePath result 
        _ -> putStrLn "You must provide exactly one argument, the path of the file to compile."

