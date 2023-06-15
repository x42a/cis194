{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Data.Map qualified as M
import Distribution.Simple.Utils (xargs)
import ExprT (ExprT (..))
import Parser (parseExp)
import StackVM qualified as S

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr = eval' . parseExp Lit Add Mul
  where
    eval' (Just x) = Just $ eval x
    eval' Nothing = Nothing

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 x) (Mod7 y) = Mod7 $ (`mod` 7) $ x + y
  mul (Mod7 x) (Mod7 y) = Mod7 $ (`mod` 7) $ x * y

testExp :: (Expr a) => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

instance Expr S.Program where
  lit x = [S.PushI x]
  add x y = x ++ y ++ [S.Add]
  mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExprT
  = VLit Integer
  | VAdd VarExprT VarExprT
  | VMul VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance HasVars VarExprT where
  var = Var

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var :: String -> M.Map String Integer -> Maybe Integer
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _ = Just x
  add x y m = add' (x m) (y m)
    where
      add' (Just x) (Just y) = Just $ x + y
      add' _ _ = Nothing
  mul x y m = mul' (x m) (y m)
    where
      mul' (Just x) (Just y) = Just $ x * y
      mul' _ _ = Nothing

withVars ::
  [(String, Integer)] ->
  (M.Map String Integer -> Maybe Integer) ->
  Maybe Integer
withVars vs exp = exp $ M.fromList vs