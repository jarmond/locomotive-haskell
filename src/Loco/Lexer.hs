-- Copyright (C) 2017 Jonathan W. Armond
module Loco.Lexer where

import Control.Monad (void)
import Control.Applicative (empty)
import Data.Char (toUpper)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Loco.Identifiers

-- | Space consumer
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt empty
  where lineCmnt = L.skipLineComment "REM"

-- | lexeme consumes whitespace after every lexeme.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol' sc

-- | Parse anything between parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = L.signed sc $ lexeme L.integer

real :: Parser Double
real = lexeme L.float

hex :: Parser Integer
hex = lexeme $ char '&' >> L.hexadecimal

colon :: Parser String
colon = symbol ":"

command :: Parser String
command = lexeme $ some letterChar

-- | Reserved word
reserved :: String -> Parser ()
reserved w = string' w *> notFollowedBy alphaNumChar *> sc

-- | Reserved word list
reservedList :: [String]
reservedList = reservedIdentifiers

identifier :: Parser String
identifier = (lexeme . try) (some letterChar >>= check)
  where
    check x = if (map toUpper x) `elem` reservedList
              then fail $ "command " ++ show x ++ " cannot be an identifier"
              else return x
