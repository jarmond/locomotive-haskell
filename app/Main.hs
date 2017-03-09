-- Copyright (C) 2017 Jonathan W. Armond
module Main where

import Loco.AST
import Loco.Parser
import Loco.Error
import Loco.Eval

import System.IO
import System.Environment (getArgs)
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  if null args then repl else runParseStatement $ args

  -- case runParseLine "10 PRINT 3" of
  --        Left err -> putStrLn err
  --        Right (CommandLine n line) -> do
  --          putStrLn "Successfully parsed:"
  --          putStrLn $ show n ++ " " ++ show line

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

readLine :: String -> LocoEval CommandLine
readLine = runParseLine

repl :: IO ()
repl = forever $ readPrompt >>= readLine >>= eval
