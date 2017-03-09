-- Copyright (C) 2017 Jonathan W. Armond
module Loco.Store where

import Loco.AST
import Loco.Error

import Control.Monad
import Control.Monad.Except
import Data.Maybe
import Data.IORef

type Program = [CommandLine]

type Store = IORef [(String, IORef LocoValue)]

newStore :: IO Store
newStore = newIORef []

isVar :: Store -> String -> IO Bool
isVar storeRef name = readIORef storeRef >>= return . isJust . lookup name

getVar :: Store -> String -> IOLocoEval LocoValue
getVar storeRef name = do
  store <- liftIO $ readIORef storeRef
  maybe (throwError $ UndeclaredVarError name)
        (liftIO . readIORef)
        (lookup name store)

setVar :: Store -> String -> LocoValue -> IO ()
setVar storeRef name val = do
  store <- liftIO $ readIORef storeRef
  valRef <- newIORef val
  case (lookup name store) of
    Nothing -> liftIO $ writeIORef storeRef ((name, valRef) : store)
    Just curRef -> liftIO $ writeIORef curRef val
