-- Copyright (C) 2017 Jonathan W. Armond
{-# LANGUAGE RankNTypes #-}
module Loco.Eval where

import Loco.Error
import Loco.AST
import Loco.Store
import Loco.Pretty

import Data.IORef
import Control.Monad.Except
import Control.Monad

type Jump = Maybe LineNumber

-- |Same as evalSt, but fakes the line number and discards any jump.
evalSt1 :: Store -> Statement -> IOLocoEval ()
evalSt1 st stmt = evalSt st 10 stmt >> return ()

-- |Evaluates (executes) a single statement and maybe returns a line number to jump to.
evalSt :: Store -> LineNumber -> Statement -> IOLocoEval Jump
evalSt st _ (Command cmd args) = mapM (eval st) args >>= command cmd
evalSt st _ (Dim (Variable name t) args) = undefined
evalSt _ _ (Dim _ _) = throwError $ TypeError "expected variable for DIM"
evalSt st linum (For (Variable name _) from to step) =
  for st linum name from to step
evalSt _ _ (For _ _ _ _) = throwError $ TypeError "expected variable for FOR"
evalSt st linum (If expr@(BoolBinary _ _ _) thenSt elseSt) =
  ifstmt st linum expr thenSt elseSt
evalSt _ _ (If _ _ _) = throwError $ TypeError "expected boolean expression for IF"
evalSt _ _ (While expr@(BoolBinary _ _ _)) = undefined
evalSt _ _ (While _) = throwError $ TypeError "expected boolean expression for WHILE"
evalSt st _ (Assign (Variable name _) expr) =
  eval st expr >>= assign st name >> return Nothing
evalSt _ _ (Assign _ _) = throwError $ TypeError "expected variable for assignment"

command :: String -> [LocoValue] -> IOLocoEval Jump
command "PRINT" (arg:_) = liftIO $ (putStrLn . prettyShow) arg >> return Nothing

-- |Execute WHILE loop.
while :: Store -> LineNumber -> LocoExpr -> IOLocoEval Jump
while st linum expr = do
  cond <- evalBool st expr
  if cond
    then getJump st WhileLoop linum >>= return . Just
    else return Nothing

-- |Execute FOR loop. Sets and updates a variable in store until 'to' condition
-- is reached, when it executes a jump to line after NEXT. If condition not
-- reached, execution will continue on next line by default after this returns.
for :: Store -> LineNumber -> String -> LocoExpr -> LocoExpr -> (Maybe LocoExpr) -> IOLocoEval Jump
for st linum name from to maybeStep = do
  alreadySet <- liftIO $ isVar st name
  if alreadySet
    -- Variable already set, increment by step and check condition.
    then do var <- getVar st name
            stepVal <- step
            var' <- liftIOEval $ locoOp (+) var stepVal
            cond <- evalBool st $ stop var'
            if cond
              then getJump st ForLoop linum >>= return . Just
              else return Nothing
    -- Set variable to initial 'from' value.
    else do fromVal <- eval st from
            assign st name fromVal
            return Nothing

  where
    -- Increment value is specified by optional STEP or default of 1.
    step = maybe ((return . Int) 1) (eval st) maybeStep
    -- Stop condition.
    stop v = BoolBinary Equal (Value v) to

ifstmt :: Store -> LineNumber -> LocoExpr -> Statement -> Statement -> IOLocoEval Jump
ifstmt st linum expr thenSt elseSt = do
  bool <- evalBool st expr
  if bool then evalSt st linum thenSt else evalSt st linum elseSt

-- |Assign a variable to store. Type safety is enforced by setvar.
assign :: Store -> String -> LocoValue -> IOLocoEval ()
assign st name val = setVar st name val

-- |Evaluate an expression.
eval :: Store -> LocoExpr -> IOLocoEval LocoValue
eval _  (Value val)          = return val
eval st var@(Variable name _) = do
  val <- getVar st name
  if matchedVarType var val
    then return val
    else throwError $ TypeError "in expression"
eval st (ArithBinary op a b) = aeval st op a b

aeval :: Store -> ABinOp -> LocoExpr -> LocoExpr -> IOLocoEval LocoValue
aeval st Add a b      = liftBinOp extractValue (locoOp (+)) (eval st a) (eval st b)
aeval st Subtract a b = liftBinOp extractValue (locoOp (-)) (eval st a) (eval st b)
aeval st Multiply a b = liftBinOp extractValue (locoOp (*)) (eval st a) (eval st b)
aeval st Divide a b   = liftBinOp extractValue locoDiv (eval st a) (eval st b)
aeval st IntDiv a b   = liftBinOp extractValue locoIntDiv (eval st a) (eval st b)

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

locoIntDiv :: LocoValue -> LocoValue -> LocoEval LocoValue
locoIntDiv (Int a) (Int b) = return $ Int (a `div` b)
locoIntDiv a b = throwError $ TypeError (show a ++ " " ++ show b)

-- |Evaluate a boolean expression to a Bool.
evalBool :: Store -> LocoExpr -> IOLocoEval Bool
evalBool st expr = eval st expr >>= bool
  where
    bool :: LocoValue -> IOLocoEval Bool
    bool (Bool b) = return b
    bool _ = throwError $ TypeError "expected boolean expression"
