-- Copyright (C) 2017 Jonathan W. Armond
{-# LANGUAGE Rank2Types #-}
module Loco.Eval where

import Loco.Error
import Loco.AST
import Loco.Store
import Loco.Commands

import Data.IORef
import Control.Monad.Except
import Control.Monad

-- |Same as evalSt, but discards any jump.
evalSt1 :: Store -> Statement -> IOLocoEval ()
evalSt1 st stmt = evalSt st stmt >> return ()

-- |Evaluates (executes) a single statement and maybe returns a line number to jump to.
evalSt :: Store -> Statement -> IOLocoEval Jump
evalSt _ (Comment _) = return Next
evalSt st (Command cmd args) = mapM (eval st) args >>= command cmd
evalSt st (Dim (Variable name t) args) = undefined
evalSt _ (Dim _ _) = throwError $ TypeError "expected variable for DIM"
evalSt st forSt@(For (Variable _ _) _ _ _ _) = for st forSt
evalSt _ (For _ _ _ _ _ ) = throwError $ TypeError "expected variable for FOR"
evalSt st (If expr@(BoolBinary _ _ _) thenSt elseSt) =
  ifstmt st expr thenSt elseSt
evalSt _ (If _ _ _) = throwError $ TypeError "expected boolean expression for IF"
evalSt st (While expr@(BoolBinary _ _ _) linum) = while st expr linum
evalSt _ (While _ _) = throwError $ TypeError "expected boolean expression for WHILE"
evalSt st (Assign (Variable name _) expr) =
  eval st expr >>= assign st name >> return Next
evalSt _ (Assign _ _) = throwError $ TypeError "expected variable for assignment"
evalSt st (LoopJump _ linum) = return $ Jump linum

-- |Execute a command.
command :: String -> [LocoValue] -> IOLocoEval Jump
command name args = maybe (throwError $ UnknownCommand name)
                          ($ args)
                          (lookupCmd name)

-- |Execute WHILE loop.
while :: Store -> LocoExpr -> LineNumber -> IOLocoEval Jump
while st cond jmp = do
  chkCond <- evalBool st cond
  if chkCond then return Next else return $ JumpNext jmp

-- |Execute FOR loop. Sets and updates a variable in store each iteration.
-- Condition is checked in LoopJump.
for :: Store -> Statement -> IOLocoEval Jump
for st forSt@(For (Variable name _) from to maybeStep jmp) = do
  alreadySet <- liftIO $ isVar st name
  if alreadySet
    -- Variable already set, increment by step and check condition.
    then do var <- getVar st name
            stepVal <- step
            var' <- liftIOEval $ locoOp (+) var stepVal
            setVar st name var'
            chkCond <- evalBool st $ cond var'
            return $ if chkCond then Next else JumpNext jmp
    -- Set variable to initial 'from' value, and check restart.
    else do fromVal <- eval st from
            setVar st name fromVal
            for st forSt
  where
    -- Increment value is specified by optional STEP or default of 1.
    step = maybe ((return . Int) 1) (eval st) maybeStep
    -- Loop condition.
    cond var = BoolBinary LessEq (Value var) to

ifstmt :: Store -> LocoExpr -> Statement -> Maybe Statement -> IOLocoEval Jump
ifstmt st expr thenSt elseSt = do
  bool <- evalBool st expr
  if bool then evalSt st thenSt
          else maybe (return Next) (evalSt st) elseSt

-- |Assign a variable to store. Type safety is enforced by setvar.
assign :: Store -> String -> LocoValue -> IOLocoEval ()
assign st name val = setVar st name val

-- |Evaluate an expression.
eval :: Store -> LocoExpr -> IOLocoEval LocoValue
eval _  (Value val)          = return val
eval st (Variable name _)    = getVar st name
eval st (ArithBinary op a b) = aeval st op a b
eval st (BoolBinary op a b) = beval st op a b
eval st (Neg expr) = eval st expr >>= liftIOEval . negateExpr
eval st (Not expr) = evalBool st expr >>= return . Bool
eval st (Function name args) = mapM (eval st) args >>= liftIOEval . (function name)

function :: String -> [LocoValue] -> LocoEval LocoValue
function name args = maybe (throwError $ UnknownCommand name)
                           ($ args)
                           (lookupFn name)


negateExpr :: LocoValue -> LocoEval LocoValue
negateExpr (Int val) = return $ Int (-val)
negateExpr (Real val) = return $ Real (-val)
negateExpr _ = throwError $ TypeError "cannot negate non-numeric expression"

aeval :: Store -> ABinOp -> LocoExpr -> LocoExpr -> IOLocoEval LocoValue
aeval st Add a b      = liftBinOp extractValue (locoOp (+)) (eval st a) (eval st b)
aeval st Subtract a b = liftBinOp extractValue (locoOp (-)) (eval st a) (eval st b)
aeval st Multiply a b = liftBinOp extractValue (locoOp (*)) (eval st a) (eval st b)
aeval st Divide a b   = liftBinOp extractValue locoDiv (eval st a) (eval st b)
aeval st IntDiv a b   = liftBinOp extractValue locoIntDiv (eval st a) (eval st b)
aeval st Mod a b      = liftBinOp extractValue locoMod (eval st a) (eval st b)

-- |Lifts a binary function f into monad n, given an unpacker u for m c.
liftBinOp :: (Monad m, Monad n) => (m c -> c) -> (a -> b -> m c) -> n a -> n b -> n c
liftBinOp u f na nb = do
  a <- na
  b <- nb
  return $ u $ f a b

locoOp :: (forall a. Num a => a -> a -> a) -> LocoValue -> LocoValue -> LocoEval LocoValue
locoOp op (Int a) (Int b) = return $ Int (a `op` b)
locoOp op (Real a) (Real b) = return $ Real (a `op` b)
locoOp op a b = throwError $ TypeError (show a ++ " op " ++ show b)

locoDiv :: LocoValue -> LocoValue -> LocoEval LocoValue
locoDiv (Int a) (Int b) = return $ Int (a `div` b)
locoDiv (Real a) (Real b) = return $ Real (a / b)
locoDiv a b = throwError $ TypeError (show a ++ " / " ++ show b)

locoIntDiv :: LocoValue -> LocoValue -> LocoEval LocoValue
locoIntDiv (Int a) (Int b) = return $ Int (a `div` b)
locoIntDiv a b = throwError $ TypeError (show a ++ " \\ " ++ show b)

locoMod :: LocoValue -> LocoValue -> LocoEval LocoValue
locoMod (Int a) (Int b) = return $ Int (a `mod` b)
locoMod a b = throwError $ TypeError (show a ++ " MOD " ++ show b)

-- |Evaluate a boolean expression to a Bool.
evalBool :: Store -> LocoExpr -> IOLocoEval Bool
evalBool st expr = eval st expr >>= bool
  where
    bool :: LocoValue -> IOLocoEval Bool
    bool (Bool b) = return b
    bool _ = throwError $ TypeError "expected boolean expression"

beval :: Store -> BBinOp -> LocoExpr -> LocoExpr -> IOLocoEval LocoValue
beval st And a b = liftBinOp extractValue (locoBoolOp (&&)) (eval st a) (eval st b)
beval st Or a b = liftBinOp extractValue (locoBoolOp (||)) (eval st a) (eval st b)
beval st Xor a b = liftBinOp extractValue (locoBoolOp xor) (eval st a) (eval st b)
beval st Greater a b = liftBinOp extractValue (locoRelOp (>)) (eval st a) (eval st b)
beval st Less a b = liftBinOp extractValue (locoRelOp (<)) (eval st a) (eval st b)
beval st GreaterEq a b = liftBinOp extractValue (locoRelOp (>=)) (eval st a) (eval st b)
beval st LessEq a b = liftBinOp extractValue (locoRelOp (<=)) (eval st a) (eval st b)
beval st Equal a b = liftBinOp extractValue (locoRelOp (==)) (eval st a) (eval st b)
beval st NotEqual a b = liftBinOp extractValue (locoRelOp (/=)) (eval st a) (eval st b)

xor True False = True
xor False True = True
xor _ _        = False

locoBoolOp :: (Bool -> Bool -> Bool) -> LocoValue -> LocoValue -> LocoEval LocoValue
locoBoolOp op a b = liftM Bool $ op <$> ba <*> bb
  where ba = toBool a
        bb = toBool b

locoRelOp :: (forall a. Ord a => a -> a -> Bool) -> LocoValue -> LocoValue -> LocoEval LocoValue
locoRelOp op (Int a) (Int b) = return $ Bool (a `op` b)
locoRelOp op (Real a) (Real b) = return $ Bool (a `op` b)
locoRelOp _ a b = throwError $ TypeError (show a ++ " " ++ show b)

toBool :: LocoValue -> LocoEval Bool
toBool (Bool b) = return b
toBool (Int x)
  | x == -1 = return True
  | x == 0  = return False
  | otherwise = throwError $ TypeError ("invalid boolean integer: " ++ show x)
toBool _ = throwError $ TypeError "boolean value must be boolean or integer"
