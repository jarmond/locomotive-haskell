-- Copyright (C) 2017 Jonathan W. Armond
module AST where

-- |Variable types
data Type = TReal | TInt | TString
  deriving (Show,Eq)

-- |Abstract syntax for variables.
data Variable = Variable Name Type
  deriving (Show,Eq)

type Name = String
-- |Abstract syntax for boolean expressions.
data LocoBExpr = Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RelOp AExpr AExpr
           | ABool AExpr -- Arithmetic expression can eval to bool
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


-- |Abstract syntax for values.
data LocoValue = Var Variable
               | Int Integer
               | Real Double
               | String String
               deriving (Show,Eq)

-- |Abstract syntax for expressions (for assignments).
-- StrCmd is for string processing commands where the command name is followed by $
data LocoExpr = Value LocoValue
              | StrCmd String [Expr]
              | Neg LocoExpr
              | ArithBinary ABinOp AExpr AExpr
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
data CommandLine = CommandLine LineNumber Statement
  deriving (Show,Eq)


