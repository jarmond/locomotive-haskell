-- Copyright (C) 2017 Jonathan W. Armond
module Loco.Error where

import Control.Monad.Except

type LocoEval  = Either LocoError
data LocoError = ArgError Int Int
               | TypeError String String
               | ParserError String
               | UnknownError String
               deriving Show

