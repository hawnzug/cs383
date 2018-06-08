{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty

import Example (exampleTests)
import Parser (parserTests)
import Eval (evalTests)

main = defaultMain $ testGroup "tests" [parserTests, evalTests, exampleTests]
