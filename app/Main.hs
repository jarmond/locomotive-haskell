-- Copyright (C) 2017 Jonathan W. Armond
module Main where

import Loco.AST
import Loco.Parser
import Loco.Error
import Loco.Eval
import Loco.Interpreter

import System.IO
import System.Environment (getArgs)
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  if null args then repl else runParseStatement $ args

readPrompt :: IO String
readPrompt = prompt >> hFlush stdout >> getLine

readLine :: String -> LocoEval CommandLine
readLine = runParseLine

readStatement :: String -> LocoEval Statement
readStatement = runParseStatement

repl :: IO ()
repl = forever $ readPrompt >>= readStatement >>= eval
