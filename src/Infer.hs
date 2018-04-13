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
  | Let EVar Expr Expr
  | Add Expr Expr
  deriving (Show, Eq)

data Type
  = TInt
  | TVar TVar
  | TArr Type Type
  deriving (Show, Eq)

data Scheme = Scheme [TVar] Type
  deriving (Show, Eq)

data TypeError
  = TypeError
  | UnboundedVar
  | UnificationFail Type Type
  | InfiniteType TVar Type
  deriving (Show, Eq)

type TVar = Int
type Subst = Map TVar Type
newtype Env = Env (Map EVar Type)
type Infer a = ExceptT TypeError (State TVar) a

class Substitutable a where
  apply    :: Subst -> a -> a
  freeVars :: a -> Set TVar

instance Substitutable Type where
  apply _ TInt = TInt
  apply s t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

  freeVars TInt = Set.empty
  freeVars (TVar a) = Set.singleton a
  freeVars (TArr t1 t2) = Set.union (freeVars t1) (freeVars t2)

instance Substitutable Scheme where
  apply s (Scheme ns t) = Scheme ns (apply s' t)
    where s' = foldr Map.delete s ns

  freeVars (Scheme ns t) = foldr Set.delete (freeVars t) ns

instance Substitutable Env where
  apply s (Env env) = Env (Map.map (apply s) env)

  freeVars (Env env) = Map.foldr (Set.union . freeVars) Set.empty env

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

instantiate ::  Scheme -> Infer Type
instantiate (Scheme ns t) = do
  ns' <- mapM (const fresh) ns
  let s = Map.fromList $ zip ns ns'
  return $ apply s t

generalize :: Env -> Type -> Scheme
generalize env t  = Scheme ns t
  where ns = Set.toList $ freeVars t `Set.difference` freeVars env

runInfer i = evalState (runExceptT i) 0

fresh :: Infer Type
fresh = do
  n <- get
  put (n+1)
  return $ TVar n

unify ::  Type -> Type -> Infer Subst
unify (e1a `TArr` e1b) (e2a `TArr` e2b)  = do
    s1 <- unify e1a e2a
    s2 <- unify (apply s1 e1b) (apply s1 e2b)
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

infer :: Env -> Expr -> Infer (Subst, Type)
infer _ (Lit _) = return (Map.empty, TInt)
infer (Env env) (Var v) = case Map.lookup v env of
  Just t -> return (Map.empty, t)
  Nothing -> throwError UnboundedVar
infer env (Add e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (apply s1 env) e2
  s3 <- unify (apply s2 t1) TInt
  s4 <- unify (apply s3 t2) TInt
  return (s4 `compose` s3 `compose` s2 `compose` s1, TInt)
infer (Env env) (Lam v body) = do
  tv <- fresh
  let env1 = Map.insert v tv env
  (s, tbody) <- infer (Env env1) body
  return $ (s, TArr (apply s tv) tbody)
infer env (App e1 e2) = do
  tv <- fresh
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (apply s1 env) e2
  s3 <- unify (apply s2 t1) (TArr t2 tv)
  return (s3 `compose` s2 `compose` s1, apply s3 tv)

haha expr = snd <$> (runInfer $ infer (Env Map.empty) expr)
