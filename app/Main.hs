module Main where

import Simpl.Parser (parseProg)
import Simpl.Eval (runEval)
import Simpl.Infer (runInfer)

main :: IO ()
main = do
  input <- readFile "expr.txt"
  case parseProg input "expr.txt" of
    Left err -> putStr err
    Right expr -> print $ runInfer expr
