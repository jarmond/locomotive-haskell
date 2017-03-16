-- Copyright (C) 2017 Jonathan W. Armond
module Loco.Parser
  ( runParseLine
  , runParseStatement
  , runParseProgram
  ) where

-- TODO convert to Text

import Loco.Error
import Loco.AST
import Loco.Lexer

import Control.Monad
import Control.Monad.State.Lazy
import Control.Applicative
import Control.Arrow (left)
import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L

-- |Parser
type Parser = StateT LoopStack (Parsec Dec String)

-- |Parser state stack
type LoopStack = [LoopCond]
newtype LoopCond = LoopCond { unLoopCond :: (LocoExpr, LineNumber) }

pop :: Parser LoopCond
pop = state (\(x:xs) -> (x,xs))

push :: LoopCond -> Parser ()
push x = state (\xs -> ((),x:xs))

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

parseFor :: LineNumber -> Parser Statement
parseFor linum = do
  try $ reserved "FOR"
  var <- parseVariable
  symbol "="
  from <- parseExpr
  reserved "TO"
  to <- parseExpr
  -- Optional STEP.
  step <- try $ optional (reserved "STEP" *> parseExpr)
  -- Push loop condition and return jump onto stack.
  let cond = BoolBinary Equal var to
  push $ LoopCond (cond, linum)
  return $ For var from to step

parseWhile :: LineNumber -> Parser Statement
parseWhile linum = do
  try $ reserved "WHILE"
  cond <- parseExpr
  push $ LoopCond (cond, linum)
  return $ While cond

parseLoopJump :: Parser Statement
parseLoopJump = do
  loop <- try (symbol "NEXT" *> return ForLoop <|> symbol "WEND" *> return WhileLoop)
  loopCond <- pop
  let (cond, linum) = unLoopCond loopCond
  return $ LoopJump loop cond linum

parseAssignment :: Parser Statement
parseAssignment = do
  -- note <* sequences actions, discarding result of second
  var <- try $ parseVariable <* symbol "="
  expr <- parseExpr
  return $ Assign var expr

-- Line number is passed onto to loop constructs so it can be stored on stack.
parseStatement :: LineNumber -> Parser Statement
parseStatement linum = parseFor linum <|>
                       parseWhile linum <|>
                       parseLoopJump <|>
                       -- parseDim <|>
                       -- parseIf <|>
                       parseHyphenSep <|>
                       parseAssignment <|>
                       parseCommaSep <?> "statement"

parseLine :: Parser CommandLine
parseLine = do
  linum <- integer
  cmd <- parseStatement linum
  return $ CommandLine linum cmd

parseProgram :: Parser Program
parseProgram = some parseLine

runParse :: Parser a -> String -> LocoEval a
runParse rule text = left (ParserError . parseErrorPretty) $ runP
  where
    runP = runParser stateWrapped "(source)" text
    stateWrapped = evalStateT rule []

runParseLine      = runParse parseLine
runParseStatement = runParse $ parseStatement 0
runParseProgram   = runParse parseProgram
