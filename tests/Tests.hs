{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit

import Simpl.Parser
import Simpl.Core

main = defaultMain parserTests

parserTests = testGroup "Parser tests"
  [ testCase "Unit, Pair, Parens" $
      parse "((()), ())" >>=
      (@?= Pair Unit Unit)
  , testCase "Sequence" $
      parse "a;b;c" >>=
      (@?= (a `Seq` b) `Seq` c)
  , testCase "Operator" $
      parse "a + b * c <= d" >>=
      (@?= (a `Add` (b `Mult` c)) `LessEq` d)
  , testCase "App Ref Deref Neg" $
      parse "~a not !b ref c" >>=
      (@?= ((Neg a) `App` (Not $ Deref b)) `App` (Ref c))
  , testCase "AndAlso OrElse" $
      parse "a > b orelse a >= c andalso b = c" >>=
      (@?= (a `Greater` b) `OrElse` ((a `GreaterEq` c) `AndAlso` (b `Eq` c)))
  , testCase "Comment and Whitespace" $
      parse "(* bla (* bla *) bla *)\n a \r\n\t(**)  " >>=
      (@?= a)
  , testCase "List Bool" $
      parse "false :: true :: nil" >>=
      (@?= f `Cons` (t `Cons` Nil))
  , testCase "Identifier" $
      parse "not' true_ _1a" >>=
      (@?= ((Var "not'") `App` (Var "true_")) `App` (Var "_1a"))
  ]
  where a = Var "a"
        b = Var "b"
        c = Var "c"
        d = Var "d"
        t = BoolLit True
        f = BoolLit False
        parse input = case parseMb input of
          Just v -> return v
          _ -> assertFailure "cannot parse"
