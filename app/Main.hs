-- Copyright (C) 2017 Jonathan W. Armond
module Main where

import Loco.Interpreter

import System.IO
import System.Environment (getArgs)
import Control.Monad
import Control.Monad.Except

main :: IO ()
main = do
  args <- getArgs
  welcome
  let filename = args !! 0
  if null args then repl else loadAndRun filename

loadAndRun filename = readFile filename >>= runProgram

readPrompt :: IO String
readPrompt = prompt >> hFlush stdout >> getLine

repl :: IO ()
repl = forever $ (liftIO readPrompt) >>= runStatement

welcome :: IO ()
welcome = putStrLn "Locomotive BASIC"
