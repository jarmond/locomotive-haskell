-- Copyright (C) 2017 Jonathan W. Armond
module Loco.Eval where

import Loco.Error
import Loco.AST

import Data.IORef
import Control.Exception (throw)
import Control.Monad

type Program = [CommandLine]

type Store = IORef [(String, IORef LocoValue)]

-- |Evaluate a command line and update store and program.
evalLine :: CommandLine -> LocoEval LocoValue
evalLine = undefined

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
locoAdd a b = throw $ TypeError (show a ++ " " ++ show b)

locoSub :: LocoValue -> LocoValue -> LocoEval LocoValue
locoSub (Int a) (Int b) = return $ Int (a - b)
locoSub (Real a) (Real b) = return $ Real (a - b)
locoSub a b = throw $ TypeError (show a ++ " " ++ show b)

locoMul :: LocoValue -> LocoValue -> LocoEval LocoValue
locoMul (Int a) (Int b) = return $ Int (a * b)
locoMul (Real a) (Real b) = return $ Real (a * b)
locoMul a b = throw $ TypeError (show a ++ " " ++ show b)

locoDiv :: LocoValue -> LocoValue -> LocoEval LocoValue
locoDiv (Int a) (Int b) = return $ Int (a `div` b)
locoDiv (Real a) (Real b) = return $ Real (a / b)
locoDiv a b = throw $ TypeError (show a ++ " " ++ show b)

