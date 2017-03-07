-- Copyright (C) 2017 Jonathan W. Armond
module Interpreter where

import Loco.Parser
import Loco.Eval
import Loco.Error

import Control.Monad


runLine :: String -> IO String
runLine line = runIOEval $ liftM show $ (liftIOEval $ runParseLine line) >>= (liftIOEval . evalLine)

runLineAndPrint :: String -> IO ()
runLineAndPrint line = runLine line >>= putStrLn
