-- Copyright (C) 2017 Jonathan W. Armond
{-# LANGUAGE Rank2Types #-}
module Loco.Commands
  ( lookupCmd
  , lookupFn
  ) where

import Loco.Error
import Loco.AST
import Loco.Pretty

import Prelude hiding (getChar)
import Control.Monad.Except
import Data.Char (chr,ord,intToDigit)
import Numeric (showIntAtBase)
import Data.Map (Map)
import qualified Data.Map as Map

-- Commands
type Command = [LocoValue] -> IOLocoEval Jump

lookupCmd :: String -> Maybe Command
lookupCmd = lookupMap commands

lookupFn :: String -> Maybe Function
lookupFn = lookupMap functions

lookupMap :: Map String a -> String -> Maybe a
lookupMap = flip Map.lookup

commands :: Map String Command
commands = Map.fromList
  [("PRINT", printCmd)
  ,("GOTO", goto)]

-- Command implementations

printCmd (arg:_) = liftIO $ (putStrLn . prettyShow) arg >> return Next

goto (arg:_) = liftIOEval $ getInt arg >>= return . Jump


-- Functions
type Function = [LocoValue] -> LocoEval LocoValue

functions :: Map String Function
functions = Map.fromList
  [("ABS", liftNum abs)
  ,("ASC", unary asc)
  ,("ATN", unary atn)
  ,("BIN$", binary bin)
  ,("CHR$", unary chr')
  ,("SQR", liftFloat sqrt)
  ]

-- |@unary f@ turns a one argument function into a multiargument function
unary :: (LocoValue -> LocoEval LocoValue) -> Function
unary f = f . head

binary :: (LocoValue -> LocoValue -> LocoEval LocoValue) -> Function
binary f = uncurry f . twoArgs
  where twoArgs (x:y:_) = (x,y)

getChar :: LocoValue -> LocoEval Char
getChar = fmap head . getStr

getStr :: LocoValue -> LocoEval String
getStr (String s) = return s
getStr _ = throwError $ TypeError "expected string value"

getReal :: LocoValue -> LocoEval Double
getReal (Int x) = return $ fromIntegral x
getReal (Real x) = return x
getReal _ = throwError $ TypeError "expected numeric value"

getInt :: LocoValue -> LocoEval Integer
getInt (Int x) = return $ fromIntegral x
getInt _ = throwError $ TypeError "expected integer value"

liftNum :: (forall a. Num a => a -> a) -> [LocoValue] -> LocoEval LocoValue
liftNum f ((Int x):_) = (return . Int) $ f x
liftNum f ((Real x):_) = (return . Real) $ f x
liftNum _ _ = throwError $ TypeError "expected numeric value"

liftFloat :: (forall a. Floating a => a -> a) -> [LocoValue] -> LocoEval LocoValue
liftFloat f ((Int x):_) = (return . Int . truncate) $ f (fromIntegral x)
liftFloat f ((Real x):_) = (return . Real) $ f x
liftFloat _ _ = throwError $ TypeError "expected numeric value"

-- Function implementations

-- fmap raises ord to LocoEval
asc = fmap (Int . fromIntegral . ord) . getChar

atn = fmap (Real . atan) . getReal

bin val width = fmap (String . showBinary) (getInt val)
  where showBinary x = showIntAtBase 2 intToDigit x ""

chr' = fmap (String . (:[]) . chr . fromIntegral) . getInt
