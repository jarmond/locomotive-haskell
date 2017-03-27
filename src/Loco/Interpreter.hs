-- Copyright (C) 2017 Jonathan W. Armond
module Loco.Interpreter where

import Loco.Parser
import Loco.Eval
import Loco.Error
import Loco.Store
import Loco.AST
import Loco.Pretty

import Control.Monad
import Control.Monad.Except
import Data.List
import Data.List.Zipper

trace = False
traceShow = show

-- |Execute a program with a new store.
execProgramNewStore :: Program -> IO ()
execProgramNewStore prog = newStore >>= execProgram prog

-- |Execute a program using the provided store.
execProgram :: Program -> Store -> IO ()
execProgram [] _    = return ()
execProgram prog st = runIOEval $ execProg st progZip
  where progZip = fromList prog

-- |Execute program. Uses a zipper to recurse down list.
execProg :: Store -> Zipper CommandLine -> IOLocoEval ()
execProg st progZip = liftIO $ unless (endp progZip) $ runIOEval exec
  where
    exec = do
      let cmdline@(CommandLine linum stmt) = cursor progZip
      -- Execute statement. If a jump is returned then execution should move
      -- to jump target line.
      when trace $ liftIO (printStore st >> (putStrLn . traceShow) cmdline)
      jump <- evalSt st stmt
      case jump of
        Jump linum -> jumpToLine 0 linum
        JumpNext linum -> jumpToLine 1 linum
        Next -> execNextLine
        where
          -- Recursively execute next line.
          execNextLine :: IOLocoEval ()
          execNextLine = execProg st $ right progZip

          -- Unpack zipper back to list and search for linenumber, optionally
          -- skipping on @skip@ lines.
          jumpToLine :: Int -> LineNumber -> IOLocoEval ()
          jumpToLine skip n = do
            let prog = toList progZip
            case findIndex ((==n) . lineNum) prog of
              Nothing -> (throwError $ InvalidLineError n)
              -- Skip to the jump.
              Just idx -> execProg st $ zipToIndex progZip (idx+skip)

lineNum :: CommandLine -> Integer
lineNum (CommandLine linum _) = linum

-- |zipToIndex moves the cursor to index i
zipToIndex :: Zipper a -> Int -> Zipper a
zipToIndex z i = iterate right (start z) !! i

runStatement :: String -> IO ()
runStatement s = runIOEval $ exec
  where exec = do
          st <- liftIO newStore
          stmt <- liftIOEval $ runParseStatement s
          evalSt1 st stmt

runProgram :: String -> IO ()
runProgram xs = runIOEval $ exec
  where exec = do
          prog <- liftIOEval $ runParseProgram xs
          liftIO $ printProgram prog
          liftIO $ prompt
          liftIO $ execProgramNewStore prog

printProgram :: Program -> IO ()
printProgram prog = mapM_ (putStrLn . prettyShow) prog

runShowProgram :: String -> IO ()
runShowProgram xs = runIOEval $ exec
  where exec = do
          prog <- liftIOEval $ runParseProgram xs
          liftIO $ showProgram prog

showProgram :: Program -> IO ()
showProgram prog = mapM_ (putStrLn . show) prog

prompt :: IO ()
prompt = putStrLn "Ready"
