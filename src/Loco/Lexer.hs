-- Copyright (C) 2017 Jonathan W. Armond
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Loco.Lexer where

import Control.Monad (void)
import Control.Applicative (empty)
import Data.Char (toUpper)
import Text.Megaparsec
import Text.Megaparsec.Prim
import qualified Text.Megaparsec.Lexer as L

import Loco.Identifiers

-- Generalizes these lexers to work in a monad transformer stack.
type GenParser = MonadParsec Dec String

-- | Space consumer
sc :: GenParser m => m ()
sc = L.space (void spaceChar) empty empty
-- sc = L.space (void spaceChar) lineCmnt empty
--   where lineCmnt = L.skipLineComment "REM"

-- | lexeme consumes whitespace after every lexeme.
lexeme :: GenParser m => m a -> m a
lexeme = L.lexeme sc

symbol :: GenParser m => String -> m String
symbol = L.symbol' sc

-- | Parse anything between parentheses.
parens :: GenParser m => m a -> m a
parens = between (symbol "(") (symbol ")")

integer :: GenParser m => m Integer
integer = L.signed sc $ lexeme L.integer

real :: GenParser m => m Double
real = lexeme L.float

hex :: GenParser m => m Integer
hex = lexeme $ char '&' >> L.hexadecimal

colon :: GenParser m => m String
colon = symbol ":"

command :: GenParser m => m String
command = lexeme $ some letterChar

-- | Reserved word
reserved :: GenParser m => String -> m ()
reserved w = string' w *> notFollowedBy alphaNumChar *> sc

genericIdentifier :: GenParser m => m Char -> m String
genericIdentifier charParser = (lexeme . try) (some charParser >>= check)
  where
    check x = if (map toUpper x) `elem` reservedIdentifiers
              then fail $ "command " ++ show x ++ " cannot be an identifier"
              else return x

identifier :: GenParser m => m String
identifier = genericIdentifier $ letterChar <|> char '$'

funcIdentifier :: GenParser m => m String
funcIdentifier = genericIdentifier letterChar
