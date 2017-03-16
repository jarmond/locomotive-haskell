-- Copyright (C) 2017 Jonathan W. Armond
{-# LANGUAGE Rank2Types #-}
module Loco.Commands where

import Loco.Error
import Loco.AST
import Loco.Pretty

import Control.Monad.Except

type Command = [LocoValue] -> IOLocoEval Jump

commandList :: [(String, Command)]
commandList = [("PRINT", printCmd)]

printCmd (arg:_) = liftIO $ (putStrLn . prettyShow) arg >> return Nothing

type Function = [LocoValue] -> LocoEval LocoValue

numericFunctionList :: [(String, Function)]
numericFunctionList = [("ABS", liftNum abs)]


liftNum :: (forall a. Num a => a -> a) -> [LocoValue] -> LocoEval LocoValue
liftNum f ((Int x):_) = (return . Int) $ f x
liftNum f ((Real x):_) = (return . Real) $ f x
