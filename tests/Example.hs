module Example where

import Test.Tasty
import Test.Tasty.HUnit

import Simpl.Parser (parseProg)
import Simpl.Infer (typeInfer, Type(..))
import Simpl.Eval (runEval)

example name tp val = 
  testCaseSteps name $ \step -> do
    input <- readFile $ "examples/"++name++".spl"
    step "Paring..."
    case parseProg input $ name++".spl" of
      Left err -> assertFailure err
      Right expr -> do
        step "Type checking..."
        case typeInfer expr of
          Left err -> assertFailure $ show err
          Right typ -> do
            tp @=? show typ
            step "Evaluating..."
            case runEval expr of
              Left err -> assertFailure $ show err
              Right v -> val @=? show v
  

exampleTests = testGroup "Examples tests"
  [ example "sum" "Int" "6"
  , example "max" "Int" "2"
  , example "map" "(t7->t8)->([t7]->[t8])" "fun"
  , example "plus" "Int" "3"
  , example "gcd1" "Int" "1029"
  , example "gcd2" "Int" "1029"
  , example "letrec" "Bool" "false"
  , example "factorial" "Int" "24"
  , example "pcf.sum" "Int->(Int->Int)" "fun"
  , example "pcf.even" "Int->Bool" "fun"
  , example "pcf.twice" "Int" "16"
  , example "pcf.minus" "Int" "46"
  , example "pcf.fibonacci" "Int" "6765"
  , example "pcf.factorial" "Int" "720"
  ]
