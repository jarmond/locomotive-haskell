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

setVar :: Store -> String -> LocoValue -> IOLocoEval ()
setVar storeRef name val = do
  store <- liftIO $ readIORef storeRef
  valRef <- liftIO $ newIORef val
  case (lookup name store) of
    -- New variable, store it.
    Nothing -> liftIO $ writeIORef storeRef ((name, valRef) : store)
    -- Existing variable, check types match, then store it.
    Just curRef -> do
      cur <- liftIO $ readIORef curRef
      if matchedTypes cur val
        then liftIO $ writeIORef curRef val
        else throwError $ TypeError "in assignment"

matchedTypes :: LocoValue -> LocoValue -> Bool
matchedTypes (Int _) (Int _)       = True
matchedTypes (Real _) (Real _)     = True
matchedTypes (String _) (String _) = True
matchedTypes _ _                   = False
