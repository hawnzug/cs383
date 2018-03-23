{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Simpl.Eval where

import Unbound.Generics.LocallyNameless
import qualified Unbound.Generics.LocallyNameless.Name as UnbName
import Control.Monad.Except
import qualified Control.Monad.State as St
import qualified Data.Vector as V
import Simpl.Core

data Error
  = ErrUnbounded
  | ErrType
  | ErrApp
  deriving (Show)

type Env = (Integer, Memory)

newtype MyFresh a = MyFresh { unMyFresh :: St.State Env a }
  deriving (Functor, Applicative, Monad)

runMyFresh (MyFresh st) = St.evalState st (0, V.empty)

instance St.MonadState Env MyFresh where
  get = MyFresh $ St.get
  put s = MyFresh $ St.put s

instance Fresh MyFresh where
  fresh (UnbName.Fn s _) = MyFresh $ do
    (n, m) <- St.get
    St.put $! (n+1, m)
    return $ (UnbName.Fn s n)
  fresh nm@(UnbName.Bn {}) = return nm

allocate :: St.MonadState Env m => Expr -> m Address
allocate e = do
  (n, m) <- St.get
  let m' = V.snoc m e
  St.put $! (n, m')
  return $ V.length m

assign :: St.MonadState Env m => Address -> Expr -> m ()
assign addr e = do
  (n, m) <- St.get
  let m' = m V.// [(addr, e)]
  St.put $! (n, m')
  return ()

deref :: St.MonadState Env m => Address -> m Expr
deref addr = do
  (_, m) <- St.get
  return $ m V.! addr

preTerms :: [NameE]
preTerms = s2n <$>
  [ "fst", "snd", "hd", "tl"
  , "iszero", "pred", "succ"
  ]

eval :: Expr -> ExceptT Error MyFresh Expr
eval e@(Var n) = if elem n preTerms
  then return e
  else throwError ErrUnbounded 
eval e@(Fn _) = return e
eval e@(Rec _) = return e
eval e@(IntLit _) = return e
eval e@(BoolLit _) = return e
eval e@(RefCell _) = return e
eval (Neg e) = eval e >>= \case
  IntLit n -> return (IntLit (negate n))
  _ -> throwError ErrType
eval (Not e) = eval e >>= \case
  BoolLit b -> return (BoolLit (not b))
  _ -> throwError ErrType
eval (Add e1 e2) = evalArith (+) e1 e2
eval (Sub e1 e2) = evalArith (-) e1 e2
eval (Mult e1 e2) = evalArith (*) e1 e2
eval (Div e1 e2) = evalArith div e1 e2
eval (Mod e1 e2) = evalArith mod e1 e2
eval (AndAlso e1 e2) = evalBool (&&) e1 e2
eval (OrElse e1 e2) = evalBool (||) e1 e2
eval (Eq e1 e2) = evalComp (==) e1 e2
eval (Neq e1 e2) = evalComp (/=) e1 e2
eval (Less e1 e2) = evalComp (<) e1 e2
eval (LessEq e1 e2) = evalComp (<=) e1 e2
eval (Greater e1 e2) = evalComp (>) e1 e2
eval (GreaterEq e1 e2) = evalComp (>=) e1 e2

eval (App e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case v1 of
    (Fn bnd) -> do
      (x, body) <- unbind bnd
      eval (subst x v2 body)
    rec@(Rec bnd) -> do
      (f, body) <- unbind bnd
      let f' = subst f rec body
      eval (App f' v2)
    (Var pre) -> evalPre pre v2
    _ -> throwError ErrApp

eval (Let bnd) = do
  ((x, e), body) <- unbind bnd
  v <- eval (unembed e)
  eval (subst x v body)

eval (Cond cond e1 e2) = do
  eval cond >>= \case
    BoolLit True  -> eval e1
    BoolLit False -> eval e2
    _ -> throwError ErrType

eval e@(Loop cond body) = do
  eval cond >>= \case
    BoolLit True -> do
      eval body
      eval e
    BoolLit False -> return Unit
    _ -> throwError ErrType

eval (Ref e) = do
  v <- eval e
  addr <- allocate v
  return $ RefCell addr

eval (Deref e) = do
  v <- eval e
  case v of
    RefCell addr -> deref addr
    _ -> throwError ErrType

eval (Assign e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case v1 of
    RefCell addr -> assign addr v2 >> return Unit
    _ -> throwError ErrType

eval (Seq e1 e2) = eval e1 >> eval e2
eval (Pair e1 e2) = Pair <$> eval e1 <*> eval e2
eval Nil = return Nil
eval (Cons e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case v2 of
    Nil -> return (Cons v1 v2)
    Cons _ _ -> return (Cons v1 v2)
    _ -> throwError ErrType

evalArith op e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (IntLit n1, IntLit n2) -> return (IntLit (op n1 n2))
    _ -> throwError ErrType
evalComp op e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (IntLit n1, IntLit n2) -> return (BoolLit (op n1 n2))
    _ -> throwError ErrType
evalBool op e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (BoolLit b1, BoolLit b2) -> return (BoolLit (op b1 b2))
    _ -> throwError ErrType

evalPre :: NameE -> Expr -> ExceptT Error MyFresh Expr
evalPre pre v | pre == s2n "iszero" = case v of
  IntLit n -> return $ BoolLit (n == 0)
  _ -> throwError ErrType
evalPre pre v | pre == s2n "succ" = case v of
  IntLit n -> return $ IntLit (n + 1)
  _ -> throwError ErrType
evalPre pre v | pre == s2n "pred" = case v of
  IntLit n -> return $ IntLit (n - 1)
  _ -> throwError ErrType
evalPre pre v | pre == s2n "fst" = case v of
  Pair v1 _ -> return v1
  _ -> throwError ErrType
evalPre pre v | pre == s2n "snd" = case v of
  Pair _ v2 -> return v2
  _ -> throwError ErrType

runEval expr = runMyFresh $ runExceptT (eval expr)
