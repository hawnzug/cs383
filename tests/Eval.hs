module Eval where

import Test.Tasty
import Test.Tasty.HUnit

import Simpl.Parser (parseMb)
import Simpl.Eval (runEval, Value(..))

evalTests = testGroup "Eval tests"
  [ testCase "Reference" $
      evalEq "let x = ref 1 in x := 2; !x+1 end" $
      Vint 3
  , testCase "Recursive" $
      evalEq "let fact = rec f => fn x => if x=1 then 1 else x * (f (x-1)) in  fact 4 end" $
      Vint 24
  , testCase "Loop" $
      evalEq "let gcd = fn x => fn y => let a = ref x in let b = ref y in let c = ref 0 in (while !b <> 0 do c := !a; a := !b; b := !c % !b); !a end end end in  gcd 34986 3087 end" $
      Vint 1029
  , testCase "Pair" $
      evalEq "(fn p => if (fst p) > (snd p) then fst p else snd p)(1,2)" $
      Vint 2
  , testCase "Builtin" $
      evalEq "let cons = fn x => fn xs => fn n => if iszero n then x else if iszero (pred n) then xs else false in let nil' = fn n => true (* This is flawed; hd nil' and tl nil' both return true! *) in let hd = fn f => f 0 in let tl = fn f => f 1 in let null = fn f => f 2 in let equal = rec e => (* This tests whether two integers are equal. *) fn a => fn b => if iszero a then iszero b else if iszero b then false else e (pred a) (pred b) in let member = rec m => fn n => fn ns => if null ns then false else if equal n (hd ns) then true else m n (tl ns) in member 4 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil'))))) end end end end end end end" $
      Vbool True
  ]
  where evalEq input expect = case parseMb input of
          Just e -> case runEval e of
            Left err -> assertFailure (show err)
            Right v -> v == expect @? "aeq"
          _ -> assertFailure "cannot parse"
