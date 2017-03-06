-- Copyright (C) 2017 Jonathan W. Armond
module Loco.Pretty where

import Parser
import Identifiers

import Data.List (intercalate,intersperse)

-- Pretty printing

fmtVariable :: Variable -> String
fmtVariable (Variable n t) = n ++ show t

-- fmtExpr :: Expr -> String
-- fmtExpr (Var v)        = show v
-- fmtExpr (Int x)        = show x
-- fmtExpr (Real x)       = show x
-- fmtExpr (String s)     = "\"" ++ s ++ "\""
-- 
-- fmtExpr (BinOp op a b) =
--   parenOpen a ++ show a ++ parenClose a ++ " " ++ show op ++ " " ++ parenOpen b ++ show b ++ parenClose b
--   where parenOpen (BinOp _ _ _)  = "("
--         parenOpen _              = ""
--         parenClose (BinOp _ _ _) = ")"
        -- parenClose _             = ""
fmtExpr (StrCmd c args) = show c ++ "$" ++ argsCommaParen args

argsComma args = intercalate "," (map show args)
argsCommaParen args = "(" ++ argsComma args ++ ")"

fmtCommand :: Statement -> String
fmtCommand (Command c args) | isHyphenCmd c = c ++ " " ++ fmtExpr (args!!0) ++ "-" ++ fmtExpr (args!!1)
                            | otherwise     = c ++ " " ++ argsComma args
fmtCommand (Dim v args) =
  "DIM " ++ show v ++ argsCommaParen args
fmtCommand (For v args) =
  "FOR " ++ show v ++ "=" ++ from ++ " TO " ++ to ++ opt
  where from = show $ args !! 0
        to   = show $ args !! 1
        opt  = show $ if length args > 2 then " STEP " ++ (show $ args!!2) else ""


fmtType :: Type -> String
fmtType TReal   = "!"
fmtType TInt    = "%"
fmtType TString = "$"
