module Main where

import Simpl.Parser (parseProg)
import Simpl.Eval (runEval)
import Simpl.Infer (typeInfer)

import Options.Applicative
import Data.Semigroup ((<>))

main :: IO ()
main = runProgram =<< execParser opts
  where
    file = strArgument (metavar "FILE")
    opts = info (file <**> helper) $
         fullDesc
      <> header "Simpl - CS383 Final Project"

runProgram :: FilePath -> IO ()
runProgram filename = do
  input <- readFile filename
  case parseProg input "expr.txt" of
    Left err -> putStr err
    Right expr -> case typeInfer expr of
      Left err -> print err
      Right t -> do
        print t
        case runEval expr of
          Left err -> print err
          Right v -> print v
