-- Copyright (C) 2017 Jonathan W. Armond
module Loco.Eval where

import Loco.Error
import Loco.Parser

type Program = [CommandLine]

type Store = IORef [(String, IORef Expr)]

-- |Evaluate a command line and update store and program.
evalLine :: CommandLine -> LocoEval Expr

-- |Evaluate an expression.
eval :: Expr -> LocoEval Expr
eval e@(String _)  = return e
eval (ArithExpr e) = liftM ArithExpr $ aeval e

aeval :: AExpr -> LocoEval AExpr
--aeval e@(Var _)  = return e Look up in store 
aeval e@(Int _)  = return e
aeval e@(Real _) = return e
aeval (ABinary Add a b)      = join $ locoAdd <$> (aeval a) <*> (aeval b)
aeval (ABinary Subtract a b) = join $ locoSub <$> (aeval a) <*> (aeval b)
aeval (ABinary Multiply a b) = join $ locoMul <$> (aeval a) <*> (aeval b)
aeval (ABinary Divide a b)   = join $ locoDiv <$> (aeval a) <*> (aeval b)

locoAdd :: AExpr -> AExpr -> LocoEval AExpr
locoAdd (Int a) (Int b) = return $ Int (a + b)
locoAdd (Real a) (Real b) = return $ Real (a + b)
locoAdd a b = throwError $ TypeError "incompatible types" [ArithExpr a,ArithExpr b]

locoSub :: AExpr -> AExpr -> LocoEval AExpr
locoSub (Int a) (Int b) = return $ Int (a - b)
locoSub (Real a) (Real b) = return $ Real (a - b)
locoSub a b = throwError $ TypeError "incompatible types" [ArithExpr a,ArithExpr b]

locoMul :: AExpr -> AExpr -> LocoEval AExpr
locoMul (Int a) (Int b) = return $ Int (a * b)
locoMul (Real a) (Real b) = return $ Real (a * b)
locoMul a b = throwError $ TypeError "incompatible types" [ArithExpr a,ArithExpr b]

locoDiv :: AExpr -> AExpr -> LocoEval AExpr
locoDiv (Int a) (Int b) = return $ Int (a `div` b)
locoDiv (Real a) (Real b) = return $ Real (a / b)
locoDiv a b = throwError $ TypeError "incompatible types" [ArithExpr a,ArithExpr b]

