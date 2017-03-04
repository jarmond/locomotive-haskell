-- Copyright (C) 2017 Jonathan W. Armond
module Main where

import Parser

main :: IO ()
main = case runParseLine "10 PRINT 3" of
         Left err -> putStrLn err
         Right (CommandLine n line) -> do
           putStrLn "Successfully parsed:"
           putStrLn $ show n ++ " " ++ show line
