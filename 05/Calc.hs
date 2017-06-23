module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add n m) = eval n + eval m
eval (Mul n m) = eval n * eval m

evalParsedExp :: Maybe ExprT -> Maybe Integer
evalParsedExp Nothing = Nothing
evalParsedExp (Just n) = Just (eval n)

evalStr :: String -> Maybe Integer
evalStr = evalParsedExp . parseExp Lit Add Mul

reify :: ExprT -> ExprT
reify = id

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit a = a > 0
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit n = MinMax n
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
  lit n = Mod7 $ mod (max 0 n) 7
  add (Mod7 a) (Mod7 b) = lit $ lit a + lit b
  mul (Mod7 a) (Mod7 b) = lit $ lit a * lit b
