-- Copyright (C) 2017 Jonathan W. Armond
module Loco.AST where

-- |Data types
data LocoType = LInt | LReal | LString
              deriving (Show,Eq)

-- |Abstract syntax for values.
data LocoValue = Int Integer
               | Real Double
               | String String
               | Bool Bool -- Not instantiable by user
               | Func LocoExpr
               deriving (Show,Eq)

-- |Abstract syntax for expressions (for assignments).
-- StrCmd is for string processing commands where the command name is followed by $
data LocoExpr = Value LocoValue
              | Variable String LocoType
              | Neg LocoExpr
              | ArithBinary ABinOp LocoExpr LocoExpr
              | Not LocoExpr
              | BoolBinary BBinOp LocoExpr LocoExpr
              | Function String [LocoExpr]
              deriving (Show,Eq)

-- |Binary boolean logic operation.
data BBinOp = And | Or | Xor | Greater | Less | GreaterEq | LessEq | Equal | NotEqual
                  deriving (Show,Eq)

-- |Binary arithmetic operation.
data ABinOp = Add | Subtract | Multiply | Divide | Mod | IntDiv
                deriving (Show,Eq)

-- |Abstract syntax for commands. Locomotive BASIC commands come in a plethora
-- of lexical formats.
data Statement = Command String [LocoExpr]
               | Dim LocoExpr [LocoExpr]
               | For LocoExpr LocoExpr LocoExpr (Maybe LocoExpr)
               | If LocoExpr Statement (Maybe Statement)
               | While LocoExpr
               | LoopJump Loop LocoExpr LineNumber
               | Assign LocoExpr LocoExpr -- TODO multiple :-separated assignment
               | Comment String
               deriving (Show,Eq)

data Loop = ForLoop | WhileLoop deriving (Show,Eq)

type Jump = Maybe LineNumber

-- Program structure
type LineNumber = Integer
type Program = [CommandLine]
data CommandLine = CommandLine LineNumber Statement
  deriving (Show,Eq)
