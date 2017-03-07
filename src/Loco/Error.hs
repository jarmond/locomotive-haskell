-- Copyright (C) 2017 Jonathan W. Armond
module Loco.Error where

import Control.Monad.Except

type LocoEval  = Either LocoError
data LocoError = ArgError Int Int
               | TypeError String
               | ParserError String
               | UnknownError String
instance Show LocoError where show = showError

showError :: LocoError -> String
showError (ArgError n m) = "expected " ++ show n ++ " args but found " ++ show m
showError (TypeError msg) = "invalid types: " ++ msg
showError (ParserError msg) = "parse error: " ++ msg
showError (UnknownError msg) = msg

type IOLocoEval = ExceptT LocoError IO

trapError action = catchError action (return . show)

runIOEval :: IOLocoEval String -> IO String
runIOEval action = runExceptT (trapError action) >>= return . extractValue

extractValue :: LocoEval a -> a
extractValue (Right val) = val

liftIOEval :: LocoEval a -> IOLocoEval a
liftIOEval (Left err) = throwError err
liftIOEval (Right val) = return val
