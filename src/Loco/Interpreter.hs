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
      let (CommandLine linum stmt) = cursor progZip
      -- Execute statement. If a jump is returned then execution should precede
      -- on line after jump point.
      maybeJump <- evalSt st linum stmt
      maybe execNextLine jumpToLine maybeJump
        where
          -- Recursively execute next line.
          execNextLine :: IOLocoEval ()
          execNextLine = execProg st $ right progZip
          -- Unpack zipper back to list and search for linenumber.
          jumpToLine :: Integer -> IOLocoEval ()
          jumpToLine n = do
            let prog = toList progZip
            case findIndex ((==n) . lineNum) prog of
              Nothing -> (throwError $ InvalidLineError n)
              -- Skip to line after the jump (i.e. idx + 1).
              Just idx -> execProg st $ zipToIndex progZip (idx + 1)

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

runProgram :: [String] -> IO ()
runProgram xs = runIOEval $ exec
  where exec = do
          prog <- liftIOEval $ parseProgram xs
          liftIO $ printProgram prog
          liftIO $ prompt
          liftIO $ execProgramNewStore prog

printProgram :: Program -> IO ()
printProgram prog = mapM_ (putStrLn . prettyShow) prog

prompt :: IO ()
prompt = putStrLn "Ready"
