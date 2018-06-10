module Parser where

import Test.Tasty
import Test.Tasty.HUnit

import Simpl.Parser (parseMb)
import Simpl.Core
import Simpl.Eval (runEval, Value(..))

parserTests = testGroup "Parser tests"
  [ testCase "Unit, Pair, Parens" $
      parseEq "((()), ())" $
      Pair Unit Unit
  , testCase "Sequence" $
      parseEq "a;b;c" $
      (a `Seq` b) `Seq` c
  , testCase "Operator" $
      parseEq "a + b * c <= d" $
      (a `Add` (b `Mult` c)) `LessEq` d
  , testCase "App Ref Deref Neg" $
      parseEq "~a not !b ref c" $
      ((Neg a) `App` (Not $ Deref b)) `App` (Ref c)
  , testCase "AndAlso OrElse" $
      parseEq "a > b orelse a >= c andalso b = c" $
      (a `Greater` b) `OrElse` ((a `GreaterEq` c) `AndAlso` (b `Eq` c))
  , testCase "Comment and Whitespace" $
      parseEq "(* bla (* bla *) bla *)\n a \r\n\t(**)  " a
  , testCase "List Bool" $
      parseEq "false :: true :: nil" $
      f `Cons` (t `Cons` Nil)
  , testCase "Identifier" $
      parseEq "not' true_ _1a" $
      ((v "not'") `App` (v "true_")) `App` (v "_1a")
  , testCase "Letrec" $
      parseEq "letrec [a=b] [b=a] in a end" $
      LetRec [("a", b), ("b", a)] a
  ]
  where a = v "a"
        b = v "b"
        c = v "c"
        d = v "d"
        v = Var
        t = BoolLit True
        f = BoolLit False
        parseEq input expect = case parseMb input of
                                 Just e -> e == expect @? "aeq"
                                 _ -> assertFailure "cannot parse"

