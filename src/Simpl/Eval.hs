{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Simpl.Eval where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Simpl.Core

data Error
  = ErrUnbounded
  | ErrApp
  | ErrEmptyList
  deriving (Show)

preTerms :: [Name]
preTerms = ["fst", "snd", "hd", "tl", "iszero", "pred", "succ"]

data Value
  = Vint  Integer
  | Vpre  Name
  | Vunit
  | Vbool Bool
  | Vref  Address
  | Vrefshow Value
  | Vpair Value Value
  | Vlist [Value]
  | Vfn   Name Expr Env
  | Vrec  Name Expr Env
  | Vdelay Expr
  deriving (Eq)

instance Show Value where
  show (Vint n) = show n
  show (Vbool True) = "true"
  show (Vbool False) = "false"
  show Vunit = "unit"
  show (Vpair x y) = "pair@" ++ show x ++ "@" ++ show y
  show (Vfn _ _ _) = "fun"
  show (Vlist []) = "nil"
  show (Vlist vs) = "list@" ++ show (length vs)
  show (Vrefshow v) = "ref@" ++ show v
  show _ = error "cannot happen"

type Address = Int
data Memory = Memory
  { memory :: IntMap Value
  , freeAddrs :: [Address]
  , allocCount :: Int
  }

allocate :: (MonadState Memory m, MonadReader Env m) => Value -> m Address
allocate v = do
  Memory{..} <- get
  when (allocCount `mod` 10 == 0) gc
  Memory{..} <- get
  let addr:rest = freeAddrs
      memory'   = if IntMap.member addr memory
                  then error "Allocate error"
                  else IntMap.insert addr v memory
  put $ Memory memory' rest (allocCount+1)
  return $ addr

gc :: (MonadState Memory m, MonadReader Env m) => m ()
gc = do
  marked <- reader markEnv
  Memory{..} <- get
  let addrs = IntSet.fromDistinctAscList $ IntMap.keys memory
      frees = IntSet.toAscList $ IntSet.difference addrs marked
      freeAddrs' = frees ++ freeAddrs
      memory' = foldl (flip IntMap.delete) memory frees
  put $ Memory memory' freeAddrs' allocCount

markEnv :: Env -> IntSet
markEnv m = IntSet.unions (mark <$> Map.elems m)

mark :: Value -> IntSet
mark (Vref a) = IntSet.singleton a
mark (Vpair x y) = IntSet.union (mark x) (mark y)
mark (Vlist xs) = IntSet.unions (mark <$> xs)
mark (Vfn _ _ env) = markEnv env
mark (Vrec _ _ env) = markEnv env
mark _ = IntSet.empty

assign :: MonadState Memory m => Address -> Value -> m ()
assign addr v = modify (\Memory{..} -> Memory (IntMap.insert addr v memory) freeAddrs allocCount)

deref :: MonadState Memory m => Address -> m Value
deref addr = gets (\(Memory m _ _) -> m IntMap.! addr)

type Env = Map.Map Name Value

eval :: ( MonadReader Env m
        , MonadError Error m
        , MonadState Memory m
        ) => Expr -> m Value
eval (Var n) = do
  reader (Map.lookup n) >>= \case
    Just (Vrec n' e env') -> local (const env') (eval $ Rec n' e)
    Just (Vdelay e) -> eval e
    Just v -> return v
    _ -> if elem n preTerms
         then return $ Vpre n
         else throwError ErrUnbounded 
eval (Fn n e) = ask >>= return . Vfn n e
eval (Rec n e) = local (\env -> Map.insert n (Vrec n e env) env) (eval e)
eval (IntLit n) = return (Vint n)
eval (BoolLit b) = return (Vbool b)
eval (Neg e) = eval e >>= \case
  Vint n -> return (Vint $ negate n)
eval (Not e) = eval e >>= \case
  Vbool b -> return (Vbool $ not b)
eval (Add e1 e2) = evalArith (+) e1 e2
eval (Sub e1 e2) = evalArith (-) e1 e2
eval (Mult e1 e2) = evalArith (*) e1 e2
eval (Div e1 e2) = evalArith div e1 e2
eval (Mod e1 e2) = evalArith mod e1 e2
eval (AndAlso e1 e2) = evalBool (&&) e1 e2
eval (OrElse e1 e2) = evalBool (||) e1 e2
eval (Eq e1 e2) = evalEqual e1 e2
eval (Neq e1 e2) = evalEqual e1 e2 >>= \case
  Vbool b -> return $ Vbool (not b)
eval (Less e1 e2) = evalComp (<) e1 e2
eval (LessEq e1 e2) = evalComp (<=) e1 e2
eval (Greater e1 e2) = evalComp (>) e1 e2
eval (GreaterEq e1 e2) = evalComp (>=) e1 e2

eval (App e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case v1 of
    Vfn  x body env -> local (const $ Map.insert x v2 env) (eval body)
    Vpre pre -> evalPre pre v2
    _ -> throwError ErrApp

eval (Let x e body) = do
  v <- eval e
  local (Map.insert x v) (eval body)

eval (LetRec binds body) = local update $ eval body
  where update env = foldl insert env binds
        insert env (name, e) = Map.insert name (Vdelay e) env

eval (Cond cond e1 e2) = do
  eval cond >>= \case
    Vbool True  -> eval e1
    Vbool False -> eval e2

eval e@(Loop cond body) = do
  eval cond >>= \case
    Vbool True -> do
      eval body
      eval e
    Vbool False -> return Vunit

eval (Ref e) = eval e >>= allocate >>= return . Vref
eval Unit = return Vunit

eval (Deref e) = do
  Vref addr <- eval e
  deref addr

eval (Assign e1 e2) = do
  Vref addr <- eval e1
  v2 <- eval e2
  assign addr v2
  return Vunit

eval (Seq e1 e2) = eval e1 >> eval e2
eval (Pair e1 e2) = Vpair <$> eval e1 <*> eval e2
eval Nil = return $ Vlist []
eval (Cons e1 e2) = do
  v1 <- eval e1
  Vlist l <- eval e2
  return $ Vlist (v1:l)


evalArith :: ( MonadState Memory m
             , MonadError Error m
             , MonadReader Env m
             ) => (Integer -> Integer -> Integer) -> Expr -> Expr -> m Value
evalArith op e1 e2 = do
  Vint n1 <- eval e1
  Vint n2 <- eval e2
  return $ Vint (op n1 n2)

evalComp :: ( MonadState Memory m
            , MonadError Error m
            , MonadReader Env m
            ) => (Integer -> Integer -> Bool) -> Expr -> Expr -> m Value
evalComp op e1 e2 = do
  Vint n1 <- eval e1
  Vint n2 <- eval e2
  return $ Vbool (op n1 n2)

evalBool :: ( MonadState Memory m
            , MonadError Error m
            , MonadReader Env m
            ) => (Bool -> Bool -> Bool) -> Expr -> Expr -> m Value
evalBool op e1 e2 = do
  Vbool b1 <- eval e1
  Vbool b2 <- eval e2
  return $ Vbool (op b1 b2)

evalEqual :: ( MonadState Memory m
             , MonadError Error m
             , MonadReader Env m
             ) => Expr -> Expr -> m Value
evalEqual e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  evalEq v1 v2

evalEq :: MonadState Memory m => Value -> Value -> m Value
evalEq v1 v2 =
  case (v1, v2) of
    (Vunit, Vunit) -> return $ Vbool True
    (Vint x, Vint y) -> return $ Vbool $ x == y
    (Vbool x, Vbool y) -> return $ Vbool $ x == y
    (Vref x, Vref y) -> do
      vx <- deref x
      vy <- deref y
      evalEq vx vy
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

evalPre :: MonadError Error m => Name -> Value -> m Value
evalPre "iszero" (Vint n) = return $ Vbool (n == 0)
evalPre "succ" (Vint n) = return $ Vint (n + 1)
evalPre "pred" (Vint n) = return $ Vint (n - 1)
evalPre "fst" (Vpair v _) = return v
evalPre "snd" (Vpair _ v) = return v
evalPre "hd" (Vlist []) = throwError ErrEmptyList
evalPre "hd" (Vlist (h:_)) = return h
evalPre "tl" (Vlist []) = throwError ErrEmptyList
evalPre "tl" (Vlist (_:t)) = return $ Vlist t
evalPre _ _ = throwError ErrUnbounded

evalRef :: MonadState Memory f => Value -> f Value
evalRef (Vref addr) = Vrefshow <$> loop addr
  where loop add = deref add >>= \case
          Vref a -> loop a
          v -> return v
evalRef v = return v

runEval :: Expr -> Either Error Value
runEval expr =
  flip evalStateT (Memory IntMap.empty [0..] 0) $
    runReaderT (eval expr >>= evalRef) Map.empty
