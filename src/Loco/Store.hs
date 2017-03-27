-- Copyright (C) 2017 Jonathan W. Armond
module Loco.Store where

import Loco.AST
import Loco.Error
import Loco.Pretty

import Control.Monad
import Control.Monad.Except
import Data.Maybe
import Data.IORef

type Store = IORef [(String, IORef LocoValue)]

newStore :: IO Store
newStore = newIORef []

isVar :: Store -> LocoExpr -> IO Bool
isVar storeRef (Variable name _) = readIORef storeRef >>= return . isJust . lookup name

-- |Retrieve variable from store.
getVar :: Store -> LocoExpr -> IOLocoEval LocoValue
getVar storeRef (Variable name _) = do
  store <- liftIO $ readIORef storeRef
  maybe (throwError $ UndeclaredVarError name)
        (liftIO . readIORef)
        (lookup name store)

-- |Sets a variable in store.
setVar :: Store -> LocoExpr -> LocoValue -> IOLocoEval ()
setVar storeRef (Variable name varType) val = do
  -- Try to coerce type if possible.
  val' <- liftIOEval $ coerceType varType val
  liftIO $ do
    store <- readIORef storeRef
    case (lookup name store) of
      -- New variable, store it.
      Nothing -> do
        valRef <- newIORef val'
        writeIORef storeRef ((name, valRef) : store)
      -- Existing variable, store it.
      Just curRef -> do
        writeIORef curRef val'

printStore :: Store -> IO ()
printStore storeRef = do
  store <- readIORef storeRef
  forM_ store printEntry
  putStrLn ""
    where printEntry (name, valRef) = do
            val <- readIORef valRef
            putStr $ name ++ ": " ++ show val ++ " "

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

coerceType :: LocoType -> LocoValue -> LocoEval LocoValue
coerceType LInt val@(Int _) = return val
coerceType LInt (Real val) = return $ Int $ truncate val
coerceType LInt val = throwError $ TypeError $ "cannot coerce " ++ show val ++ " to integer"
coerceType LReal val@(Real _) = return val
coerceType LReal (Int val) = return $ Real $ fromIntegral val
coerceType LReal val = throwError $ TypeError $ "cannot coerce " ++ show val ++ " to real"
coerceType LString val@(String _) = return val
coerceType LString val = throwError $ TypeError $ "cannot coerce " ++ show val ++ " to string"
