-- Copyright (C) 2017 Jonathan W. Armond
module Loco.Error where

import Control.Monad.Except

type LocoEval  = Either LocoError
data LocoError = ArgError Int Int
               | TypeError String
               | ParserError String
               | UnknownError String
instance Show LocoError where show = showError
instance Error LocoError where
  noMsg = UnknownError "unknown error"
  strMsg = UnknownError

showError :: LocoError -> String
showError (ArgError n m) = "expected " ++ show n ++ " args but found " ++ show m
showError (TypeError msg) = "invalid types: " ++ msg
showError (ParserError msg) = "parse error: " ++ msg
showError (UnknownError msg) = msg
