module Simpl.Core where

type Name = String

data Expr

  = IntLit Integer
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Mod Expr Expr

  | BoolLit Bool
  | AndAlso Expr Expr
  | OrElse Expr Expr
  | Eq Expr Expr
  | Neq Expr Expr
  | Less Expr Expr
  | LessEq Expr Expr
  | Greater Expr Expr
  | GreaterEq Expr Expr
  | Not Expr

  | Nil
  | Cons Expr Expr

  | Ref Expr
  | Assign Expr Expr
  | Deref Expr

  | Fn Name Expr
  | Rec Name Expr
  | App Expr Expr

  | Unit
  | Var Name
  | Pair Expr Expr

  | Seq Expr Expr
  | Let Name Expr Expr
  | LetRec [(Name, Expr)] Expr
  | Cond Expr Expr Expr
  | Loop Expr Expr
  deriving (Show, Eq)
