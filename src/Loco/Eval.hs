-- Copyright (C) 2017 Jonathan W. Armond
module Loco.Eval where

import Loco.Error
import Loco.AST

import Data.IORef
import Control.Monad.Except (throwError)
import Control.Monad


-- |Evaluate a command line and update store and program.
  -- TODO Take Store
evalLine :: CommandLine -> LocoEval LocoValue
evalLine (CommandLine linum st) = evalSt st

evalSt :: Statement -> LocoEval LocoValue
evalSt (Command cmd args) = undefined
evalSt (Dim (Variable name t) args) = undefined
evalSt (Dim _ _) = throwError $ TypeError "expected variable for DIM"
evalSt (For (Variable name t) from to step) = undefined
evalSt (For _ _ _ _) = throwError $ TypeError "expected variable for FOR"
evalSt (If expr@(BoolBinary _ _ _) thenSt elseSt) = undefined
evalSt (If _ _ _) = throwError $ TypeError "expected boolean expression for IF"
evalSt (While expr@(BoolBinary _ _ _)) = undefined
evalSt (While _) = throwError $ TypeError "expected boolean expression for WHILE"
evalSt (Assign (Variable name t) expr) = undefined
evalSt (Assign _ _) = throwError $ TypeError "expected variable for assignment"

-- |Evaluate an expression.
eval :: LocoExpr -> LocoEval LocoValue
eval (Value val)  = return val
eval (ArithBinary op a b) = aeval op a b

aeval :: ABinOp -> LocoExpr -> LocoExpr -> LocoEval LocoValue
aeval Add a b      = join $ locoAdd <$> (eval a) <*> (eval b)
aeval Subtract a b = join $ locoSub <$> (eval a) <*> (eval b)
aeval Multiply a b = join $ locoMul <$> (eval a) <*> (eval b)
aeval Divide a b   = join $ locoDiv <$> (eval a) <*> (eval b)

locoAdd :: LocoValue -> LocoValue -> LocoEval LocoValue
locoAdd (Int a) (Int b) = return $ Int (a + b)
locoAdd (Real a) (Real b) = return $ Real (a + b)
locoAdd a b = throwError $ TypeError (show a ++ " " ++ show b)

locoSub :: LocoValue -> LocoValue -> LocoEval LocoValue
locoSub (Int a) (Int b) = return $ Int (a - b)
locoSub (Real a) (Real b) = return $ Real (a - b)
locoSub a b = throwError $ TypeError (show a ++ " " ++ show b)

locoMul :: LocoValue -> LocoValue -> LocoEval LocoValue
locoMul (Int a) (Int b) = return $ Int (a * b)
locoMul (Real a) (Real b) = return $ Real (a * b)
locoMul a b = throwError $ TypeError (show a ++ " " ++ show b)

locoDiv :: LocoValue -> LocoValue -> LocoEval LocoValue
locoDiv (Int a) (Int b) = return $ Int (a `div` b)
locoDiv (Real a) (Real b) = return $ Real (a / b)
locoDiv a b = throwError $ TypeError (show a ++ " " ++ show b)

