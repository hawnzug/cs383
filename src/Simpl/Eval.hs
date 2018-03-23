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
  | ErrList
  deriving (Show)

type Env = (Integer, Memory)

newtype MyFresh s a = MyFresh { unMyFresh :: St.State s a }
  deriving (Functor, Applicative, Monad)

runMyFresh (MyFresh st) = St.evalState st (0, V.empty)

instance St.MonadState s (MyFresh s) where
  get = MyFresh $ St.get
  put s = MyFresh $ St.put s

instance Fresh (MyFresh Env) where
  fresh (UnbName.Fn s _) = MyFresh $ do
    (n, m) <- St.get
    St.put $! (n+1, m)
    return $ (UnbName.Fn s n)
  fresh nm@(UnbName.Bn {}) = return nm

allocate :: St.MonadState Env m => Value -> m Address
allocate v = do
  (n, m) <- St.get
  let m' = V.snoc m v
  St.put $! (n, m')
  return $ V.length m

assign :: St.MonadState Env m => Address -> Value -> m ()
assign addr v = do
  (n, m) <- St.get
  let m' = m V.// [(addr, v)]
  St.put $! (n, m')
  return ()

deref :: St.MonadState Env m => Address -> m Value
deref addr = do
  (_, m) <- St.get
  return $ m V.! addr

preTerms :: [NameE]
preTerms = s2n <$>
  [ "fst", "snd", "hd", "tl"
  , "iszero", "pred", "succ"
  ]

eval :: Expr -> ExceptT Error (MyFresh Env) Value
eval (Value v) = return v
eval (Var n) = if elem n preTerms
  then return $ Vpre n
  else throwError ErrUnbounded 
eval (Fn b) = return $ Vfn b
eval (Rec b) = return $ Vrec b
eval (IntLit n) = return (Vint n)
eval (BoolLit b) = return (Vbool b)
eval (Neg e) = eval e >>= \case
  Vint n -> return (Vint $ negate n)
  _ -> throwError ErrType
eval (Not e) = eval e >>= \case
  Vbool b -> return (Vbool $ not b)
  _ -> throwError ErrType
eval (Add e1 e2) = evalArith (+) e1 e2
eval (Sub e1 e2) = evalArith (-) e1 e2
eval (Mult e1 e2) = evalArith (*) e1 e2
eval (Div e1 e2) = evalArith div e1 e2
eval (Mod e1 e2) = evalArith mod e1 e2
eval (AndAlso e1 e2) = evalBool (&&) e1 e2
eval (OrElse e1 e2) = evalBool (||) e1 e2
eval (Eq e1 e2) = evalEqual e1 e2
eval (Neq e1 e2) = do
  Vbool b <- evalEqual e1 e2
  return $ Vbool (not b)
eval (Less e1 e2) = evalComp (<) e1 e2
eval (LessEq e1 e2) = evalComp (<=) e1 e2
eval (Greater e1 e2) = evalComp (>) e1 e2
eval (GreaterEq e1 e2) = evalComp (>=) e1 e2

eval (App e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case v1 of
    Vfn bnd -> do
      (x, body) <- unbind bnd
      eval (subst x (Value v2) body)
    rec@(Vrec bnd) -> do
      (f, body) <- unbind bnd
      let f' = subst f (Value rec) body
      eval (App f' (Value v2))
    Vpre pre -> evalPre pre v2
    _ -> throwError ErrApp

eval (Let bnd) = do
  ((x, e), body) <- unbind bnd
  v <- eval (unembed e)
  eval (subst x (Value v) body)

eval (Cond cond e1 e2) = do
  eval cond >>= \case
    Vbool True  -> eval e1
    Vbool False -> eval e2
    _ -> throwError ErrType

eval e@(Loop cond body) = do
  eval cond >>= \case
    Vbool True -> do
      eval body
      eval e
    Vbool False -> return Vunit
    _ -> throwError ErrType

eval (Ref e) = do
  v <- eval e
  addr <- allocate v
  return $ Vref addr

eval (Deref e) = do
  v <- eval e
  case v of
    Vref addr -> deref addr
    _ -> throwError ErrType

eval (Assign e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case v1 of
    Vref addr -> assign addr v2 >> return Vunit
    _ -> throwError ErrType

eval (Seq e1 e2) = eval e1 >> eval e2
eval (Pair e1 e2) = Vpair <$> eval e1 <*> eval e2
eval Nil = return $ Vlist []
eval (Cons e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case v2 of
    Vlist l -> return $ Vlist (v1:l)
    _ -> throwError ErrType

evalArith op e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (Vint n1, Vint n2) -> return $ Vint (op n1 n2)
    _ -> throwError ErrType
evalComp op e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (Vint n1, Vint n2) -> return $ Vbool (op n1 n2)
    _ -> throwError ErrType
evalBool op e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (Vbool n1, Vbool n2) -> return $ Vbool (op n1 n2)
    _ -> throwError ErrType
evalEqual e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  evalEq v1 v2
evalEq v1 v2 =
  case (v1, v2) of
    (Vunit, Vunit) -> return $ Vbool True
    (Vint x, Vint y) -> return $ Vbool $ x == y
    (Vbool x, Vbool y) -> return $ Vbool $ x == y
    (Vref x, Vref y) -> do
      v1 <- deref x
      v2 <- deref y
      evalEq v1 v2
    (Vlist x, Vlist y) ->
      if length x == length y
      then do
        result <- mapM (uncurry evalEq) (zip x y)
        return $ Vbool $ all (\case
          Vbool True -> True
          _ -> False) result
      else return $ Vbool False
    (Vpair x1 y1, Vpair x2 y2) -> do
      Vbool b1 <- evalEq x1 x2
      Vbool b2 <- evalEq y1 y2
      return $ Vbool $ b1 && b2
    _ -> return $ Vbool False

evalPre :: NameE -> Value -> ExceptT Error (MyFresh Env) Value
evalPre pre v | pre == s2n "iszero" = case v of
  Vint n -> return $ Vbool (n == 0)
  _ -> throwError ErrType
evalPre pre v | pre == s2n "succ" = case v of
  Vint n -> return $ Vint (n + 1)
  _ -> throwError ErrType
evalPre pre v | pre == s2n "pred" = case v of
  Vint n -> return $ Vint (n - 1)
  _ -> throwError ErrType
evalPre pre v | pre == s2n "fst" = case v of
  Vpair v1 _ -> return v1
  _ -> throwError ErrType
evalPre pre v | pre == s2n "snd" = case v of
  Vpair _ v2 -> return v2
  _ -> throwError ErrType
evalPre pre v | pre == s2n "hd" = case v of
  Vlist [] -> throwError ErrList
  Vlist (h:_) -> return h
  _ -> throwError ErrType
evalPre pre v | pre == s2n "tl" = case v of
  Vlist [] -> throwError ErrList
  Vlist (_:t) -> return (Vlist t)
  _ -> throwError ErrType

runEval expr = runMyFresh $ runExceptT (eval expr)
