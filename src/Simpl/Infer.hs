{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Simpl.Infer where

import Unbound.Generics.LocallyNameless
import Control.Monad.Except
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Simpl.Core
import Data.Map (Map)
import qualified Data.Map as Map


data Type
  = TInt
  | TVar (Name Type)
  | TArr Type Type
  deriving (Show, Generic, Typeable)

instance Alpha Type

instance Subst Type Type where
  isvar (TVar v) = Just (SubstName v)
  isvar _ = Nothing

data TypeError
  = UnboundVariable (Name Expr)
  | GenericTypeError
  deriving (Show)

type TypeEnv = Map (Name Expr) Type
type SubstEnv = Map (Name Type) Type

compose :: SubstEnv -> SubstEnv -> SubstEnv
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

apply :: SubstEnv -> Type -> Type
apply s TInt = TInt
apply s (TVar v) = case Map.lookup v s of
  Just t -> t
  _ -> TVar v
apply s (TArr t1 t2) = TArr (apply s t1) (apply s t2)

type Constraint = (Type, Type)
type Infer = ExceptT TypeError FreshM

unify :: Type -> Type -> Infer SubstEnv
unify (TArr f1 x1) (TArr f2 x2) = do
  s1 <- unify f1 f2
  s2 <- unify (apply s1 x1) (apply s1 x2)
  return (compose s2 s1)
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify TInt TInt = Map.empty
unify t1 t2 = throwError $ UnificationFail t1 t2

freshtv :: Infer Type
freshtv = TVar <$> fresh (s2n "_tv")

infer :: Env -> Expr -> Infer (Type, [Constraint])
infer env expr = case expr of
  IntLit _ -> return (TInt, [])

  Var n -> do
    case Map.lookup n env of
      Nothing -> throwError $ UnboundVariable n
      Just t  -> return (t, [])

  Fn b -> do
    (n,e) <- unbind b
    tv <- freshtv
    let env' = Map.insert n tv env
    (t, cs) <- infer env' e
    return (TArr tv t, cs)

  App e1 e2 -> do
    (t1, cs1) <- infer env e1
    (t2, cs2) <- infer env e2
    tv <- freshtv
    return (tv, (t1, TArr t2 tv) : cs1 ++ cs2)

runInfer expr = runFreshM $ runExceptT (infer Map.empty expr)
