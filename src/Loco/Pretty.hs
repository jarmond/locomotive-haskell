-- Copyright (C) 2017 Jonathan W. Armond
module Loco.Pretty
  ( prettyShow
  ) where

import Loco.AST
import Loco.Identifiers

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

-- Pretty printing

instance Pretty LocoType where
  pPrint LInt    = char '%'
  pPrint LReal   = char '!'
  pPrint LString = char '$'

instance Pretty LocoValue where
  pPrint (Int val)        = integer val
  pPrint (Real val)       = double val
  pPrint (String val)     = doubleQuotes $ text val
  pPrint (Func name expr) = text name <> pPrint expr

instance Pretty LocoExpr where
  pPrint (Value val) = pPrint val
  pPrint (Variable name t) = text name <> pPrint t
  pPrint (StrCmd name exprs) = text name <> commaArgs exprs
  pPrint (Neg expr) = char '-' <> pPrint expr
  pPrint (ArithBinary op expr1 expr2) = pPrint expr1 <+> pPrint op <+> pPrint expr2
  pPrint (Not expr) = text "NOT" <+> pPrint expr
  pPrint (BoolBinary op expr1 expr2) = pPrint expr1 <+> pPrint op <+> pPrint expr2

instance Pretty ABinOp where
  pPrint Add      = char '+'
  pPrint Subtract = char '-'
  pPrint Multiply = char '*'
  pPrint Divide   = char '/'
  pPrint Mod      = char '%'

instance Pretty BBinOp where
  pPrint And = text "AND"
  pPrint Or = text "OR"
  pPrint Xor = text "XOR"
  pPrint Greater = char '>'
  pPrint Less = char '<'
  pPrint GreaterEq = text ">="
  pPrint LessEq = text "<="
  pPrint Equal = char '='
  pPrint NotEqual = text "<>"

instance Pretty Statement where
  pPrint (Command name exprs) = text name <> commaArgs exprs
  pPrint (Dim var exprs) = text "DIM" <+> pPrint var <> commaArgs exprs
  pPrint (For var from to step) =
    text "FOR" <+> pPrint var <> char '=' <> pPrint from <+> text "TO"
    <+> pPrint to <+> maybe empty pStep step
    where pStep stepExpr = text "STEP" <+> pPrint stepExpr
  pPrint (If expr thenSt elseSt) =
    text "IF" <+> pPrint expr <+> text "THEN" <+> pPrint thenSt
    <+> text "ELSE" <+> pPrint elseSt
  pPrint (While expr) = text "WHILE" <+> pPrint expr
  pPrint (Assign var expr) = pPrint var <+> char '=' <+> pPrint expr
  pPrint (LoopJump loop (BoolBinary Equal var _) _) = pPrint loop <+> pPrint var

commaArgs exprs = parens $ hcat $ punctuate (char ',') $ map pPrint exprs

instance Pretty Loop where
  pPrint ForLoop = text "NEXT"
  pPrint WhileLoop = text "WEND"

instance Pretty CommandLine where
  pPrint (CommandLine linum stmt) = integer linum <+> pPrint stmt
