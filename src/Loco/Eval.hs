-- Copyright (C) 2017 Jonathan W. Armond
{-# LANGUAGE RankNTypes #-}
module Loco.Eval where

import Loco.Error
import Loco.AST
import Loco.Store

import Data.IORef
import Control.Monad.Except
import Control.Monad

type Jump = Maybe LineNumber

-- |Same as evalSt, but discards any jump.
evalSt1 :: Store -> Statement -> IOLocoEval ()
evalSt1 st stmt = evalSt st stmt >> return ()

-- |Evaluates (executes) a single statement and maybe returns a line number to jump to.
evalSt :: Store -> Statement -> IOLocoEval Jump
evalSt _ (Command cmd args) = undefined
evalSt _ (Dim (Variable name t) args) = undefined
evalSt _ (Dim _ _) = throwError $ TypeError "expected variable for DIM"
evalSt _ (For (Variable name t) from to step) = undefined
evalSt _ (For _ _ _ _) = throwError $ TypeError "expected variable for FOR"
evalSt _ (If expr@(BoolBinary _ _ _) thenSt elseSt) = undefined
evalSt _ (If _ _ _) = throwError $ TypeError "expected boolean expression for IF"
evalSt _ (While expr@(BoolBinary _ _ _)) = undefined
evalSt _ (While _) = throwError $ TypeError "expected boolean expression for WHILE"
evalSt st (Assign var@(Variable _ _) expr) =
  eval st expr >>= assign st var >> return Nothing
evalSt _ (Assign _ _) = throwError $ TypeError "expected variable for assignment"

-- command :: String -> [LocoExpr]

-- while :: Store -> LineNumber

assign :: Store -> LocoExpr -> LocoValue -> IOLocoEval ()
assign st (Variable name LInt) val@(Int _) = liftIO $ setVar st name val >> return ()

-- |Evaluate an expression.
eval :: Store -> LocoExpr -> IOLocoEval LocoValue
eval _  (Value val)          = return val
eval st (Variable name t)    = getVar st name
eval st (ArithBinary op a b) = aeval st op a b

aeval :: Store -> ABinOp -> LocoExpr -> LocoExpr -> IOLocoEval LocoValue
aeval st Add a b      = liftBinOp extractValue (locoOp (+)) (eval st a) (eval st b)
aeval st Subtract a b = liftBinOp extractValue (locoOp (-)) (eval st a) (eval st b)
aeval st Multiply a b = liftBinOp extractValue (locoOp (*)) (eval st a) (eval st b)
aeval st Divide a b   = liftBinOp extractValue locoDiv (eval st a) (eval st b)

-- |Lifts a binary function f into monad n, given an unpacker u for m c.
liftBinOp :: (Monad m, Monad n) => (m c -> c) -> (a -> b -> m c) -> n a -> n b -> n c
liftBinOp u f na nb = do
  a <- na
  b <- nb
  return $ u $ f a b

locoOp :: (forall a. Num a => a -> a -> a) -> LocoValue -> LocoValue -> LocoEval LocoValue
locoOp op (Int a) (Int b) = return $ Int (a `op` b)
locoOp op (Real a) (Real b) = return $ Real (a `op` b)
locoOp _ a b = throwError $ TypeError (show a ++ " " ++ show b)

locoDiv :: LocoValue -> LocoValue -> LocoEval LocoValue
locoDiv (Int a) (Int b) = return $ Int (a `div` b)
locoDiv (Real a) (Real b) = return $ Real (a / b)
locoDiv a b = throwError $ TypeError (show a ++ " " ++ show b)

