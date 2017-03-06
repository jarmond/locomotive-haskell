-- Copyright (C) 2017 Jonathan W. Armond
module Loco.AST where

-- |Abstract syntax for values.
data LocoValue = Int Integer
               | Real Double
               | String String
               deriving (Show,Eq)

-- |Abstract syntax for expressions (for assignments).
-- StrCmd is for string processing commands where the command name is followed by $
data LocoExpr = Value LocoValue
              | Variable String LocoValue
              | StrCmd String [LocoExpr]
              | Neg LocoExpr
              | ArithBinary ABinOp LocoExpr LocoExpr
              | RelBinary RelOp LocoExpr LocoExpr
              | Not LocoExpr
              | BoolBinary BBinOp LocoExpr LocoExpr
              deriving (Show,Eq)

-- |Binary boolean logic operation.
data BBinOp = And | Or | Xor
                  deriving (Show,Eq)

-- |Relation logic operation.
data RelOp = Greater | Less | GreaterEq | LessEq | Equal | NotEqual
           deriving (Show,Eq)

-- |Binary arithmetic operation.
data ABinOp = Add | Subtract | Multiply | Divide | Mod
                deriving (Show,Eq)

-- |Abstract syntax for commands. Locomotive BASIC commands come in a plethora
-- of lexical formats.
data Statement = Command String [LocoExpr]
               | Dim LocoExpr [LocoExpr]
               | For LocoExpr [LocoExpr]
               | If LocoExpr Statement Statement
               | While LocoExpr
               | Assign LocoExpr LocoExpr
               | Empty
               deriving (Show,Eq)

-- Program structure
type LineNumber = Integer
data CommandLine = CommandLine LineNumber Statement
  deriving (Show,Eq)


