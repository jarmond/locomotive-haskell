-- Copyright (C) 2017 Jonathan W. Armond
module Interpreter where

import Loco.Parser
import Loco.Eval
import Loco.Error
import Loco.Store
import Loco.AST

import Control.Monad
import Control.Monad.Except
import Data.List
import Data.List.Zipper


runNewProgram :: Program -> IO ()
runNewProgram prog = newStore >>= runProgram prog

runProgram :: Program -> Store -> IO ()
runProgram [] _    = return ()
runProgram prog st = trapError $ execProg st progZip
  where progZip = fromList prog

-- |Execute program. Uses a zipper to recurse down list.
execProg :: Store -> Zipper CommandLine -> IOLocoEval ()
execProg st progZip = liftIO $ unless (endp progZip) exec
  where
    exec = do
      let (CommandLine _ stmt) = cursor progZip
      -- Execute statement
      maybeJump <- runIOEval $ evalSt st stmt
      runIOEval $ maybe execNextLine jumpToLine maybeJump
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
              Just idx -> execProg st $ zipToIndex progZip idx

lineNum :: CommandLine -> Integer
lineNum (CommandLine linum _) = linum

-- |zipToIndex moves the cursor to index i
zipToIndex :: Zipper a -> Int -> Zipper a
zipToIndex z i = iterate right (start z) !! i

runStatement :: String -> IO ()
runStatement s = do
  st <- newStore
  stmt <- runIOEval $ liftIOEval $ runParseStatement s
  runIOEval $ evalSt1 st stmt
  return ()

