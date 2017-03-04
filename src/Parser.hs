-- Copyright (C) 2017 Jonathan W. Armond
module Parser
  ( runParseLine
  , Type(..)
  , Variable(..)
  , BExpr(..)
  , BBinOp(..)
  , RelOp(..)
  , AExpr(..)
  , ABinOp(..)
  , Expr(..)
  , Statement(..)
  , CommandLine(..)
  ) where

-- TODO convert to Text

import Lexer

import Control.Monad
import Control.Monad.Identity (Identity)
import Control.Arrow (left)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L


data Type = TReal | TInt | TString
  deriving (Show,Eq)

-- |Abstract syntax for variables.
data Variable = Variable Name Type
  deriving (Show,Eq)

type Name = String
-- |Abstract syntax for boolean expressions.
data BExpr = Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RelOp AExpr AExpr
           | ABool AExpr -- Arithmetic expression can eval to bool
              deriving (Show,Eq)

data BBinOp = And | Or | Xor
                  deriving (Show,Eq)
data RelOp = Greater | Less | GreaterEq | LessEq | Equal | NotEqual
           deriving (Show,Eq)

data ABinOp = Add | Subtract | Multiply | Divide | Mod
                deriving (Show,Eq)

-- | Abstract syntax for arithmetic expressions.
data AExpr = Var Variable
          | Int Integer
          | Real Double
          | Neg AExpr
          | ABinary ABinOp AExpr AExpr
          deriving (Show,Eq)

-- |Abstract syntax for expressions (for assignments).
-- StrCmd is for string processing commands where the command name is followed by $
data Expr = String String
          | StrCmd String [Expr]
          | ArithExpr AExpr
          deriving (Show,Eq)

-- |Abstract syntax for commands.
--
--  Locomotive BASIC commands come in a plethora of lexical formats. This is all
--  dealt with in parsing and constructors are for retaining the information for
--  printing.

data Statement = Command String [Expr]
               | Dim Variable [Expr]
               | For Variable [Expr]
               | If BExpr Statement Statement
               | While BExpr
               | Assign Variable Expr
               | Empty
               deriving (Show,Eq)

-- Program structure
type LineNumber = Integer
type Program = [CommandLine]
data CommandLine = CommandLine LineNumber Statement
  deriving (Show,Eq)

-- Parser

varTypes = "!%$"

varType :: Maybe Char -> Type
varType = maybe TReal $ \c -> case c of
                                '!' -> TReal
                                '%' -> TInt
                                '$' -> TString

parseType :: Parser Type
parseType = lexeme $ optional (oneOf varTypes) >>= return . varType 

-- |Variables in Locomotive BASIC consist of letters followed directly by an
-- optional type declaration.
parseVariable :: Parser Variable
parseVariable = do
  n <- try identifier
  t <- parseType
  return $ Variable n t

parseVar = parseVariable >>= return . Var

-- |Strings are delimited by ".
parseString :: Parser Expr
parseString = do
  try $ char '"'
  s <- manyTill anyChar (try (symbol "\""))
  return $ String s

parseStrCmd :: Parser Expr
parseStrCmd = do
  cmd <- try $ (symbol "STR" <|> symbol "CHR") <* symbol "$"
  args <- parseArgs
  return $ StrCmd cmd args

parseBExpr :: Parser BExpr
parseBExpr = makeExprParser bTerm bOperators

bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [Prefix (Not <$ reserved "NOT")]
  , [InfixL (BBinary And <$ reserved "AND")
    ,InfixL (BBinary Or  <$ reserved "OR")
    ,InfixL (BBinary Xor <$ reserved "XOR")]]

bTerm :: Parser BExpr
bTerm = parens parseBExpr <|> (liftM ABool) parseAExpr <|> parseRExpr

-- | Parser for boolean relations expressions.
parseRExpr :: Parser BExpr
parseRExpr = do
      left <- parseAExpr
      op <- relation
      right <- parseAExpr
      return $ RBinary op left right

relation :: Parser RelOp
relation = (symbol ">" *> pure Greater)
           <|> (symbol "<" *> pure Less)
           <|> (symbol ">=" *> pure GreaterEq)
           <|> (symbol "<=" *> pure LessEq)
           <|> (symbol "=" *> pure Equal)
           <|> (symbol "<>" *> pure NotEqual)

parseAExpr :: Parser AExpr
parseAExpr = makeExprParser aTerm aOperators

aOperators :: [[Operator Parser AExpr]]
aOperators =
 [ [Prefix (Neg <$ symbol "-")]
  , [InfixL (ABinary Multiply <$ symbol "*")
  ,  InfixL (ABinary Divide   <$ symbol "/")]
  , [InfixL (ABinary Add      <$ symbol "+")
  ,  InfixL (ABinary Subtract <$ symbol "-")]]

aTerm :: Parser AExpr
aTerm = parens parseAExpr
  <|> Var   <$> parseVariable
  <|> Int   <$> try integer
  <|> Real  <$> real

parseExpr :: Parser Expr
parseExpr = parseString <|> parseStrCmd <|> (liftM ArithExpr) parseAExpr

-- |Parse an argument list.
parseArgs :: Parser [Expr]
parseArgs = parseExpr `sepBy` (symbol ",")

-- Parsing statements:
--  CommaSep has 0 or more comma-separated argument, plus a second clause
--  with a single argument
--  HyphenSep has 2 arguments separated by a hyphen.
--  Dim is a special case consisting of a typed variable and comma-separated list of
--  dimensions.
--  For is a special case of the format e.g. FOR I=1 TO 10 STEP 2

-- |Parse commands with 0 or more comma-separated arguments, optionally enclosed
-- in parentheses.
parseCommaSep :: Parser Statement
parseCommaSep = do
  cmd <- command
  args <- parseArgs
  return $ Command cmd args

parseHyphenSep :: Parser Statement
parseHyphenSep = do
  cmd <- try (symbol "DELETE" <|> symbol "LIST")
  left <- parseExpr
  symbol "-"
  right <- parseExpr
  return $ Command cmd [left,right]

parseDim :: Parser Statement
parseDim = undefined

parseFor  :: Parser Statement
parseFor = do
  try $ reserved "FOR"
  var <- parseVariable
  symbol "="
  begin <- parseExpr
  reserved "TO"
  end <- parseExpr
  -- TODO optional STEP
  return $ For var [begin,end]

parseAssignment :: Parser Statement
parseAssignment = do
  -- note <* sequences actions, discarding result of second
  var <- try $ parseVariable <* symbol "="
  expr <- parseExpr
  return $ Assign var expr


-- |Parse a statement. Handle special cases first since it is easy to match on the string. Comma-separated arguments is the least identifiable
-- command so has to come last in the combinator.
-- parseStatement :: Parser Statement
-- parseStatement = parseFor <|> parseHyphenSep <|> do
--   -- Resolve between command and assignment.
--   optional (symbol "LET") -- LET is optional for assignment and ignored.
--   ident <- identifier
--   -- If not a variable assignment for sure, try to parse a command statement.
--   (notFollowedBy (oneOf "$!%") >> spaces >> parseCommaSep ident)
--   -- If there is a space and another identifier, is a multiword command.
--     <|> (some (some spaces >> some letter) >>=
--          \idlist -> parseCommaSep $ intercalate " " (ident:idlist))
--   -- Else it must be an assignment.
--     <|> parseAssignment ident

parseStatement :: Parser Statement
parseStatement = parseFor <|>
                 -- parseDim <|>
                 -- parseIf <|>
                 parseAssignment <|>
                 parseHyphenSep <|>
                 parseCommaSep <?> "statement"

parseLine :: Parser CommandLine
parseLine = do
  lineNum <- integer
  cmd <- parseStatement
  return $ CommandLine lineNum cmd


type ParserError = ParseError (Token String) Dec

runParse :: Parser a -> String -> Either String a
runParse rule text = left parseErrorPretty $ parse rule "(source)" text

runParseLine = runParse parseLine
