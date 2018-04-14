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
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Monoid (Any(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List ((\\))

import Simpl.Core

type NameT = Name Type

data Type
  = TInt
  | TVar NameT
  | TArr Type Type
  deriving (Generic, Typeable)

instance Show Type where
  showsPrec _ TInt = showString "Int"
  showsPrec d (TVar n) = showString $ show n
  showsPrec d (TArr t1 t2) = showParen (d > 10) $
    showsPrec 11 t1 . showString "->" . showsPrec 11 t2

newtype Scheme = Scheme (Bind [NameT] Type) deriving (Show, Generic, Typeable)
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
type Infer a = ReaderT Env (WriterT [Constraint] (ExceptT TypeError FreshM)) a

runInfer :: Infer a -> Either TypeError (a, [Constraint])
runInfer i =
  runFreshM $ runExceptT $ runWriterT $ runReaderT i $ Env Map.empty

envInsert :: (MonadReader Env m) => NameE -> Scheme -> m a -> m a
envInsert x s m = do
  let f (Env e) = Env $ Map.insert x s e
  local f m

freshTV :: (Fresh m) => m Type
freshTV = TVar <$> (fresh $ s2n "t")

instantiate :: (Fresh m) => Scheme -> m Type
instantiate (Scheme bd) = do
  (ns, t) <- unbind bd
  ns' <- mapM (const freshTV) ns
  return $ substs (zip ns ns') t
generalize :: Env -> Type -> Scheme
generalize (Env env) t = Scheme (bind ns t)
  where ns = freeVars t \\ freeVars (Map.elems env)

infer :: (MonadReader Env m,
          MonadWriter [Constraint] m,
          MonadError TypeError m,
          Fresh m) =>
         Expr -> m Type
infer (IntLit _) = return TInt
infer (Var v) = do
  Env env <- ask
  case Map.lookup v env of
    Just scheme -> instantiate scheme
    Nothing -> throwError UnboundedVar
infer (Fn bd) = do
  (v, body) <- unbind bd
  tv <- freshTV
  tbody <- envInsert v (Scheme (bind [] tv)) (infer body)
  return $ TArr tv tbody
infer (App e1 e2) = do
  t1 <- infer e1
  t2 <- infer e2
  tv <- freshTV
  tell [(t1, t2 `TArr` tv)]
  return tv
infer (Add e1 e2) = do
  t1 <- infer e1
  t2 <- infer e2
  tell [(t1, TInt), (t2, TInt)]
  return TInt
infer (Let bd) = do
  ((x, e), body) <- unbind bd
  env <- ask
  t1 <- infer (unembed e)
  let s1 = generalize env t1
  t2 <- envInsert x s1 (infer body)
  return t2

type Solver a = StateT (Type, [Constraint]) (Except TypeError) a

unify :: Type -> Type -> Solver ()
unify TInt TInt = return ()
unify (TVar v) t = unifyVar v t
unify t (TVar v) = unifyVar v t
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
