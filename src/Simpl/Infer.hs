{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Simpl.Infer where

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold (foldMapOf, toListOf)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Data.Monoid (Any(..), (<>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (union, (\\))

import Simpl.Core

type NameT = Name Type

data Type
  = TInt
  | TBool
  | TUnit
  | TVar NameT
  | TPair Type Type
  | TArr Type Type
  | TList Type
  | TRef Type
  deriving (Generic, Typeable)

instance Show Type where
  showsPrec _ TInt = showString "Int"
  showsPrec _ TBool = showString "Bool"
  showsPrec _ TUnit = showString "()"
  showsPrec _ (TVar n) = showString $ show n
  showsPrec d (TRef t) = showString "ref" . showsPrec d t
  showsPrec d (TPair t1 t2) =
    showString "(" .
    showsPrec d t1 .
    showString ", " .
    showsPrec d t2 . 
    showString ")"
  showsPrec d (TArr t1 t2) = showParen (d > 10) $
    showsPrec 11 t1 . showString "->" . showsPrec 11 t2
  showsPrec d (TList t) =
    showString "[" .
    showsPrec d t .
    showString "]"

newtype Scheme = Scheme (Bind [NameT] (Type, [Constraint])) deriving (Show, Generic, Typeable)
newtype Env = Env (Map NameE Scheme)

instance Alpha Type
instance Alpha Scheme

instance Subst Type Type where
  isvar (TVar x) = Just (SubstName x)
  isvar _ = Nothing

occurs :: NameT -> Type -> Bool
occurs x t = getAny $ foldMapOf fv (Any . (== x)) t

freeVars :: (Alpha a) => a -> [NameT]
freeVars = toListOf fv

data TypeError
  = TypeError
  | UnboundedVar
  | UnificationFail Type Type
  | InfiniteType NameT Type
  deriving Show

type Constraint = (Type, Type)
type Infer a = ReaderT Env (ExceptT TypeError FreshM) a

runInfer :: Infer a -> Either TypeError a
runInfer i = runFreshM $ runExceptT $ runReaderT i $ Env Map.empty

envInsert :: (MonadReader Env m) => NameE -> Scheme -> m a -> m a
envInsert x s m = do
  let f (Env e) = Env $ Map.insert x s e
  local f m

freshTV :: (Fresh m) => m Type
freshTV = TVar <$> (fresh $ s2n "t")

instantiate :: (Fresh m) => Scheme -> m (Type, [Constraint])
instantiate (Scheme bd) = snd <$> unbind bd

generalize :: Env -> [Constraint] -> Type -> Scheme
generalize (Env env) cs t = Scheme (bind ns (t, cs'))
  where ns = (freeVars t `union` freeVars cs) \\ freeVars (Map.elems env)
        cs' = filter (\c -> any (`elem` freeVars c) ns) cs

inferBinop e1 e2 to tr = do
  (t1, cs1) <- infer e1
  (t2, cs2) <- infer e2
  return (tr, (t1, to):(t2, to):cs1 <> cs2)
  
infer :: (MonadReader Env m,
          MonadError TypeError m,
          Fresh m) =>
         Expr -> m (Type, [Constraint])
infer (IntLit _) = return (TInt, [])
infer (Neg e) = do
  (t, cs) <- infer e
  return (TInt, (t, TInt):cs)
infer (Add e1 e2) = inferBinop e1 e2 TInt TInt
infer (Sub e1 e2) = inferBinop e1 e2 TInt TInt
infer (Mult e1 e2) = inferBinop e1 e2 TInt TInt
infer (Div e1 e2) = inferBinop e1 e2 TInt TInt
infer (Mod e1 e2) = inferBinop e1 e2 TInt TInt
infer (BoolLit _) = return (TBool, [])
infer (AndAlso e1 e2) = inferBinop e1 e2 TBool TBool
infer (OrElse e1 e2) = inferBinop e1 e2 TBool TBool
-- TODO Eq/Neq forall type
infer (Eq e1 e2) = inferBinop e1 e2 TInt TBool
infer (Neq e1 e2) = inferBinop e1 e2 TInt TBool
infer (Less e1 e2) = inferBinop e1 e2 TInt TBool
infer (LessEq e1 e2) = inferBinop e1 e2 TInt TBool
infer (Greater e1 e2) = inferBinop e1 e2 TInt TBool
infer (GreaterEq e1 e2) = inferBinop e1 e2 TInt TBool
infer (Not e) = do
  (t, cs) <- infer e
  return (TBool, (t, TBool):cs)
infer Nil = do
  tv <- freshTV
  return $ (TList tv, [])
infer (Cons e1 e2) = do
  (t1, cs1) <- infer e1
  (t2, cs2) <- infer e2
  return (TList t1, (t2, TList t1):cs1 <> cs2)
infer (Ref e) = do
  (t, cs) <- infer e
  return (TRef t, cs)
infer (Assign e1 e2) = do
  (reft, cs1) <- infer e1
  (t, cs2) <- infer e2
  return (TUnit, (reft, TRef t):cs1 <> cs2)
infer (Deref e) = do
  (reft, cs) <- infer e
  t <- freshTV
  return (t, (reft, TRef t):cs)
infer (Fn bd) = do
  (v, body) <- unbind bd
  tv <- freshTV
  (tbody, cs) <- envInsert v (Scheme (bind [] (tv, []))) (infer body)
  return $ (TArr tv tbody, cs)
infer (Rec bd) = do
  (v, body) <- unbind bd
  tv <- freshTV
  (tbody, cs) <- envInsert v (Scheme (bind [] (tv, []))) (infer body)
  return (tbody, (tv, tbody):cs)
infer (App e1 e2) = do
  (t1, cs1) <- infer e1
  (t2, cs2) <- infer e2
  tv <- freshTV
  return (tv, (t1, t2 `TArr` tv):cs1 <> cs2)
infer Unit = return (TUnit, [])
infer (Var v) = do
  Env env <- ask
  case Map.lookup v env of
    Just scheme -> instantiate scheme
    Nothing -> throwError UnboundedVar
infer (Pair e1 e2) = do
  (t1, cs1) <- infer e1
  (t2, cs2) <- infer e2
  return (TPair t1 t2, cs1 <> cs2)
infer (Seq e1 e2) = do
  (t1, cs1) <- infer e1
  (t2, cs2) <- infer e2
  return (t2, cs1 <> cs2)
infer (Let bd) = do
  ((x, e), body) <- unbind bd
  env <- ask
  (t1, cs1) <- infer (unembed e)
  let s1 = generalize env cs1 t1
  (t2, cs2) <- envInsert x s1 (infer body)
  return (t2, cs1 <> cs2)
infer (Cond cond e1 e2) = do
  (tc, cs0) <- infer cond
  (t1, cs1) <- infer e1
  (t2, cs2) <- infer e2
  return (t1, (tc, TBool):(t1, t2):cs0 <> cs1 <> cs2)
infer (Loop cond e) = do
  (tc, cs1) <- infer cond
  (_,  cs2) <- infer e
  return (TUnit, (tc, TBool):cs1 <> cs2)
infer (Value _) = error "Cannot happen"

type Solver a = StateT (Type, [Constraint]) (Except TypeError) a

unify :: Type -> Type -> Solver ()
unify TInt TInt = return ()
unify TBool TBool = return ()
unify TUnit TUnit = return ()
unify (TVar v) t = unifyVar v t
unify t (TVar v) = unifyVar v t
unify (TPair t1a t2a) (TPair t1b t2b) = do
  (r, cs) <- get
  put (r, (t1a, t1b) : (t2a, t2b) : cs)
unify (TList t1) (TList t2) = unify t1 t2
unify (TRef t1) (TRef t2) = unify t1 t2
unify (TArr t1a t2a) (TArr t1b t2b) = do
  (r, cs) <- get
  put (r, (t1a, t1b) : (t2a, t2b) : cs)
unify t1 t2 = throwError $ UnificationFail t1 t2

unifyVar :: NameT -> Type -> Solver ()
unifyVar x (TVar y) | x == y = return ()
unifyVar x t = if occurs x t then throwError (InfiniteType x t) else do
  (r, cs) <- get
  let r'  = subst x t r
      cs' = subst x t cs
  put (r', cs')

solve :: Solver ()
solve = do
  (t, cs) <- get
  case cs of
    [] -> return ()
    ((t1, t2):cs') -> do
      put (t, cs')
      unify t1 t2
      solve

typeInfer :: Expr -> Either TypeError Type
typeInfer expr = do
  s <- runInfer (infer expr)
  (_, (t, _)) <- runExcept (runStateT solve s)
  return t
