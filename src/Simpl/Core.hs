{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Simpl.Core where

import Data.Vector (Vector)
import Unbound.Generics.LocallyNameless
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

type NameE = Name Expr

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
  | RefCell Address -- It's value
  | Assign Expr Expr
  | Deref Expr

  | Fn (Bind NameE Expr)
  | Rec (Bind NameE Expr)
  | App Expr Expr

  | Unit
  | Var NameE
  | Pair Expr Expr

  | Seq Expr Expr
  | Let (Bind (NameE, Embed Expr) Expr)
  | Cond Expr Expr Expr
  | Loop Expr Expr
  deriving (Show, Generic, Typeable)

instance Alpha Expr

instance Subst Expr Expr where
  isvar (Var x) = Just (SubstName x)
  isvar _       = Nothing

type Address = Int
type Memory = Vector Expr
