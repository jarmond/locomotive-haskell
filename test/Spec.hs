-- Copyright (C) 2017 Jonathan W. Armond
import Parser

import Test.HUnit
import Data.Either

main :: IO ()
main = do
  runTestTT lineTests
  runTestTT parseTests
  return ()

-- |Run parsing line test on parser.
lineTest line = assertBool msg $ isRight $ runParseLine line
  where msg = "parse error on: " ++ line
-- |Helper for defining line tests.
lineCase = TestCase . lineTest


lineTests = TestList $ map lineCase
  -- Commmands
  ["10 AFTER 10,1 GOSUB 999"
  ,"20 AUTO 10,20"
  ,"30 CALL 0"
  ,"40 CAT"
  ,"50 DATA 12,12,43,54"
  ,"60 DATA &1D,&AB,&FF,&00"
  ,"70 DELETE 10-20"
  ,"80 DIM alpha%(10,10,3)"
  ,"81 DIM beta!(1,1)"
  ,"82 DIM gamma$(9,9,9,9)"
  ,"83 ERASE gamma$"
  ,"100 FOR I=1 TO 10"
  ,"110 FOR J=-10 TO 0"
  ,"120 FOR II=10 TO 0 STEP -1"
  ,"200 IF f=10 THEN PRINT \"it is 10\""
  ,"210 IF g>15 THEN GOTO 10: END: ELSE GOTO 20"
  ,"250 PRINT MID$(a$,2,2)"

  -- Assignments
  ,"10 abc=1.5"
  ,"15 def!=3.14"
  ,"20 ghi%=5000"
  ,"30 jkl$=\"test\""
  ,"40 var=(1+(6/2)) - 2"
  ]


-- |Parse line and check AST.
parseTest :: String -> CommandLine -> Assertion
parseTest line expected = assertEqual line (Right expected) $ runParseLine line
-- |Helper for defining parse tests.
parseCase line expected = TestCase $ parseTest line expected
  -- note, fmap . fmap :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
  --       let f = (->) c, f1 = (->) d
  --       hence (a -> b) -> (c -> d -> a) -> c -> d -> b

parseTests = TestList $ map (uncurry parseCase)
  -- Arithmetic expressions
  [("10 a = 1", CommandLine 10 (Assign (Variable "a" TReal) (ArithExpr (Int 1))))
  ,("10 b = 1+2", CommandLine 10 (Assign (Variable "b" TReal) (ArithExpr (ABinary Add (Int 1) (Int 2)))))
  ,("10 b = 1+2+3", CommandLine 10 (Assign (Variable "b" TReal) (ArithExpr (ABinary Add (ABinary Add (Int 1) (Int 2)) (Int 3)))))
  ,("10 b = (1+2)+3", CommandLine 10 (Assign (Variable "b" TReal) (ArithExpr (ABinary Add (ABinary Add (Int 1) (Int 2)) (Int 3)))))
  ,("10 b = 1+(2+3)", CommandLine 10 (Assign (Variable "b" TReal) (ArithExpr (ABinary Add (Int 1) (ABinary Add (Int 2) (Int 3))))))
  ,("10 b = 1/(2*3)", CommandLine 10 (Assign (Variable "b" TReal) (ArithExpr (ABinary Divide (Int 1) (ABinary Multiply (Int 2) (Int 3))))))
  ,("10 b = 1-(2-3)", CommandLine 10 (Assign (Variable "b" TReal) (ArithExpr (ABinary Subtract (Int 1) (ABinary Subtract (Int 2) (Int 3))))))

  -- Control structures
  ,("10 FOR a%=1 TO 10", CommandLine 10 (For (Variable "a" LInt) (Value (Int 1)) (Value (Int 10)) Nothing))
  ,("FOR a%=1 TO 10 STEP 2", CommandLine 10 (For (Variable "a" LInt) (Value (Int 1)) (Value (Int 10)) (Just (Value (Int 2)))))

  ]
