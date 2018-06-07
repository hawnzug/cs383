module Main where

import Simpl.Parser (parseProg)
import Simpl.Eval (runEval)
import Simpl.Infer (typeInfer)

import System.Directory (listDirectory)

main :: IO ()
main = do
  listDirectory "examples" >>= mapM_ (\file -> do
    let filename = "examples/" ++ file
    input <- readFile $ filename
    print filename
    case parseProg input "expr.txt" of
      Left err -> putStr err
      Right expr -> case typeInfer expr of
        Left err -> print err
        Right t -> do
          putStr "Type checks: "
          print t
          case runEval expr of
            Left err -> print err
            Right v -> print v
    putStrLn "")

