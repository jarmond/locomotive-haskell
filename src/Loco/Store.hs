-- Copyright (C) 2017 Jonathan W. Armond
module Loco.Store where

import Loco.AST
import Loco.Error

import Control.Monad
import Control.Monad.Except
import Data.Maybe
import Data.IORef

type Store = IORef [(String, IORef LocoValue)]

newStore :: IO Store
newStore = newIORef []

isVar :: Store -> String -> IO Bool
isVar storeRef name = readIORef storeRef >>= return . isJust . lookup name

-- |Retrieve variable from store.
getVar :: Store -> String -> IOLocoEval LocoValue
getVar storeRef name = do
  store <- liftIO $ readIORef storeRef
  maybe (throwError $ UndeclaredVarError name)
        (liftIO . readIORef)
        (lookup name store)

-- |Sets a variable in store. Variables preceded by '_' are internal to the
-- interpreter and not allowed as user variables.
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

matchedVarType :: LocoExpr -> LocoValue -> Bool
matchedVarType (Variable _ LInt) (Int _)       = True
matchedVarType (Variable _ LReal) (Real _)     = True
matchedVarType (Variable _ LString) (String _) = True
matchedVarType (Variable _ _) _                = False
matchedVarType _ _ = error "cannot match types for non-variable expression"
