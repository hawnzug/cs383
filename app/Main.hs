module Main where

import Simpl.Parser (parseProg)
import Simpl.Eval (runEval)
import Simpl.Infer (typeInfer)

import System.Directory (listDirectory)

main :: IO ()
main = do
  listDirectory "examples" >>= mapM_ (\file -> do
    putStrLn file
    let filename = "examples/" ++ file
    input <- readFile $ filename
    case parseProg input "expr.txt" of
      Left err -> putStr err
      Right expr -> case typeInfer expr of
        Left err -> print err
        Right t -> do
          print t
          case runEval expr of
            Left err -> print err
            Right v -> print v
    putStrLn "")
