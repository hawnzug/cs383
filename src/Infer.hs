module Infer where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Except
import Control.Monad.State

type EVar = String

data Expr
  = Var EVar
  | Lit Int
  | Lam EVar Expr
  | App Expr Expr
  | Add Expr Expr
  deriving (Show, Eq)

data Type
  = TInt
  | TVar TVar
  | TArr Type Type
  deriving (Show, Eq)

data TypeError
  = TypeError
  | UnboundedVar
  | UnificationFail Type Type
  | InfiniteType TVar Type
  deriving (Show, Eq)

type TVar = Int
type Subst = Map TVar Type
type Env = Map EVar Type
type Infer a = ExceptT TypeError (State TVar) a

subst :: Subst -> Type -> Type
subst _ TInt = TInt
subst s t@(TVar a) = Map.findWithDefault t a s
subst s (t1 `TArr` t2) = subst s t1 `TArr` subst s t2

substEnv :: Subst -> Env -> Env
substEnv s = Map.map (subst s)

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (subst s1) s2 `Map.union` s1

runInfer i = evalState (runExceptT i) 0

fresh :: Infer Type
fresh = do
  n <- get
  put (n+1)
  return $ TVar n

unify ::  Type -> Type -> Infer Subst
unify (e1a `TArr` e1b) (e2a `TArr` e2b)  = do
    s1 <- unify e1a e2a
    s2 <- unify (subst s1 e1b) (subst s1 e2b)
    return (s2 `compose` s1)
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify TInt TInt = return Map.empty
unify t1 t2 = throwError $ UnificationFail t1 t2
bind ::  TVar -> Type -> Infer Subst
bind a (TVar b) | a == b = return Map.empty
bind a t | occurs a t = throwError $ InfiniteType a t
         | otherwise  = return $ Map.singleton a t
occurs :: TVar -> Type -> Bool
occurs a t = Set.member a (freeVars t)
freeVars :: Type -> Set TVar
freeVars TInt = Set.empty
freeVars (TVar a) = Set.singleton a
freeVars (TArr t1 t2) = Set.union (freeVars t1) (freeVars t2)

infer :: Env -> Expr -> Infer (Subst, Type)
infer _ (Lit _) = return (Map.empty, TInt)
infer env (Var v) = case Map.lookup v env of
  Just t -> return (Map.empty, t)
  Nothing -> throwError UnboundedVar
infer env (Add e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (substEnv s1 env) e2
  s3 <- unify (subst s2 t1) TInt
  s4 <- unify (subst s3 t2) TInt
  return (s4 `compose` s3 `compose` s2 `compose` s1, TInt)
infer env (Lam v body) = do
  tv <- fresh
  let env1 = Map.insert v tv env
  (s, tbody) <- infer env1 body
  return $ (s, TArr (subst s tv) tbody)
infer env (App e1 e2) = do
  tv <- fresh
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (substEnv s1 env) e2
  s3 <- unify (subst s2 t1) (TArr t2 tv)
  return (s3 `compose` s2 `compose` s1, subst s3 tv)

haha expr = snd <$> (runInfer $ infer Map.empty expr)
