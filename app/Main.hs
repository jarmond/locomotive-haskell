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
import Control.Monad.Except

main :: IO ()
main = do
  args <- getArgs
  welcome
  let filename = args !! 1
  if null args then repl else loadFile filename >>= runProgram

loadFile :: FilePath -> IO [String]
loadFile filename = readFile filename >>= return . lines

readPrompt :: IO String
readPrompt = prompt >> hFlush stdout >> getLine

repl :: IO ()
repl = forever $ (liftIO readPrompt) >>= runStatement

welcome :: IO ()
welcome = putStrLn "Locomotive BASIC"
