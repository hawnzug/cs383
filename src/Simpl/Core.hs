module Simpl.Core where

import Data.Text.Lazy (Text)

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

  | Fn Text Expr
  | Rec Text Expr
  | App Expr Expr

  | Unit
  | Var Text
  | Pair Expr Expr

  | Seq Expr Expr
  | Let Text Expr Expr
  | Cond Expr Expr Expr
  | Loop Expr Expr
  deriving (Show, Eq)
