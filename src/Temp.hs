{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
import Data.Functor.Foldable

data ExprF a
  = Var String
  | Lit Int
  | Lam String a
  | App a a
  | Prim BinOp a a
  deriving (Functor)

data BinOp = Plus | Mult

type Expr = Fix ExprF

var = Fix . Var
lit = Fix . Lit
lam s x = Fix $ Lam s x
app x y = Fix $ App x y
prim op x y= Fix $ Prim op x y

eval :: Expr -> Int
eval = cata $ \case
  Lit n -> n
  Prim Plus x y -> x + y
  Prim Mult x y -> x * y
  _ -> 0

main :: IO ()
main = print $ eval (prim Plus (lit 5) (lit 10))
