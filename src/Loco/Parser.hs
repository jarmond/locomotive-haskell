-- Copyright (C) 2017 Jonathan W. Armond
module Loco.Parser
  ( runParseLine
  , runParseStatement
  , parseProgram
  ) where

-- TODO convert to Text

import Loco.Error
import Loco.AST
import Loco.Lexer

import Control.Monad
import Control.Applicative
import Control.Arrow (left)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

-- Parser

-- Parsing values

-- |Strings are delimited by ".
parseString :: Parser LocoValue
parseString = do
  try $ char '"'
  s <- manyTill anyChar (try (symbol "\""))
  return $ String s


-- Parsing expressions

varTypes = "!%$"

varType :: Maybe Char -> LocoType
varType = maybe LReal $ \c -> case c of
                                '!' -> LReal
                                '%' -> LInt
                                '$' -> LString

parseType :: Parser LocoType
parseType = lexeme $ optional (oneOf varTypes) >>= return . varType 

-- |Variables in Locomotive BASIC consist of letters followed directly by an
-- optional type declaration.
parseVariable :: Parser LocoExpr
parseVariable = do
  n <- try identifier
  t <- parseType
  return $ Variable n t

parseStrCmd :: Parser LocoExpr
parseStrCmd = do
  cmd <- try $ (symbol "STR" <|> symbol "CHR") <* symbol "$"
  args <- parseArgs
  return $ StrCmd cmd args

parseBExpr :: Parser LocoExpr
parseBExpr = makeExprParser bTerm bOperators

bOperators :: [[Operator Parser LocoExpr]]
bOperators =
  [ [Prefix (Not <$ reserved "NOT")]
  , [InfixL (BoolBinary And       <$ reserved "AND")
    ,InfixL (BoolBinary Or        <$ reserved "OR")
    ,InfixL (BoolBinary Xor       <$ reserved "XOR")
    ,InfixL (BoolBinary Greater   <$ reserved ">")
    ,InfixL (BoolBinary Less      <$ reserved "<")
    ,InfixL (BoolBinary GreaterEq <$ reserved ">=")
    ,InfixL (BoolBinary LessEq    <$ reserved "<=")
    ,InfixL (BoolBinary Equal     <$ reserved "=")
    ,InfixL (BoolBinary NotEqual  <$ reserved "<>")]]

bTerm :: Parser LocoExpr
bTerm = parens parseBExpr <|> parens parseAExpr

parseAExpr :: Parser LocoExpr
parseAExpr = makeExprParser aTerm aOperators

aOperators :: [[Operator Parser LocoExpr]]
aOperators =
 [ [Prefix (Neg <$ symbol "-")]
  , [InfixL (ArithBinary Multiply <$ symbol "*")
  ,  InfixL (ArithBinary Divide   <$ symbol "/")]
  , [InfixL (ArithBinary Add      <$ symbol "+")
  ,  InfixL (ArithBinary Subtract <$ symbol "-")]]

aTerm :: Parser LocoExpr
aTerm = parens parseAExpr
  <|> parseVariable
  <|> (Value . Int)   <$> try integer
  <|> (Value . Real)  <$> real

parseExpr :: Parser LocoExpr
parseExpr = (liftM Value) parseString <|> parseStrCmd <|> parseAExpr

-- |Parse an argument list.
parseArgs :: Parser [LocoExpr]
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
  left <- integer
  symbol "-"
  right <- integer
  return $ Command cmd (map (Value . Int) [left,right])

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
  -- Optional STEP.
  step <- try $ optional (reserved "STEP" *> parseExpr)
  -- Look ahead to NEXT command. This indicates the jump back the start of the
  -- loop.
  jump <- try $ lookAhead $ parseLoop "NEXT"
  return $ For var begin end step jump

parseAssignment :: Parser Statement
parseAssignment = do
  -- note <* sequences actions, discarding result of second
  var <- try $ parseVariable <* symbol "="
  expr <- parseExpr
  return $ Assign var expr

-- | Loops are delimited by a jumping statement. This parser consumes lines until
-- finding the appropriate jump, then returns the line number. It should be used
-- within try and lookahead.
parseLoop :: String -> Parser LineNumber
parseLoop loop = do
  manyTill' skipLine loopCmd
  where
    loopCmd = do
      linum <- integer
      reserved loop
      return linum

-- | @manyTill' p end@ Similar to @manyTill p end@, this returns the result of
-- @end@ instead of @p@.
manyTill' :: Alternative m => m a -> m end -> m end
manyTill' p end = go where go = end <|> (p *> go)

skipLine = manyTill anyChar eol

parseStatement :: Parser Statement
parseStatement = parseFor <|>
                 -- parseDim <|>
                 -- parseIf <|>
                 parseHyphenSep <|>
                 parseAssignment <|>
                 parseCommaSep <?> "statement"

parseLine :: Parser CommandLine
parseLine = do
  lineNum <- integer
  cmd <- parseStatement
  return $ CommandLine lineNum cmd


runParse :: Parser a -> String -> LocoEval a
runParse rule text = left (ParserError . parseErrorPretty) $ parse rule "(source)" text

runParseLine = runParse parseLine
runParseStatement = runParse parseStatement

parseProgram :: [String] -> LocoEval Program
parseProgram = mapM runParseLine
