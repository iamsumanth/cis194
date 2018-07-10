{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Spring13.Week5.Calc
  ( eval, evalStr, reify, testExp, Expr
  ) where

import Spring13.Week5.ExprT
import Spring13.Week5.Parser
import Spring13.Week5.VarExprT
import qualified Data.Map as M
  --   data ExprT = Lit Integer
  --          | Add ExprT ExprT
  --          | Mul ExprT ExprT
  -- deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit integer) = integer
eval (Add expr1 expr2) = eval expr1 + eval expr2
eval (Mul expr1 expr2) = eval expr1 * eval expr2

evalStr :: String -> Maybe Integer
evalStr = getValue . parseExp Lit Add Mul

getValue :: Maybe ExprT -> Maybe Integer
getValue Nothing = Nothing
getValue (Just expr) = Just (eval expr)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

class HasVars a where
  var :: String -> a
  
instance Expr ExprT where
  lit a = Lit a
  add expr1 expr2 = Add expr1 expr2
  mul expr1 expr2 = Mul expr1 expr2

instance Expr Integer where
  lit a = a
  add a b = (+) a b
  mul a b = (*) a b

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit input = MinMax input
  add (MinMax input1) (MinMax input2) = MinMax (max input1 input2)
  mul (MinMax input1) (MinMax input2) = MinMax (min input1 input2)

instance Expr Mod7 where
  lit input = Mod7 (mod input 7)
  add (Mod7 input1) (Mod7 input2) = Mod7 ((\x -> x `mod` 7) $ (+) input1 input2)
  mul (Mod7 input1) (Mod7 input2) = Mod7 ((\x -> x `mod` 7) $ (*) input1 input2)

instance Expr VarExprT where
  lit a = VLit a
  add expr1 expr2 = VAdd expr1 expr2
  mul expr1 expr2 = VMul expr1 expr2

instance HasVars VarExprT where
  var str = VVar str

type MapExpr = M.Map String Integer -> Maybe Integer

instance HasVars (MapExpr) where
  var str = M.lookup str


instance Expr (MapExpr) where
  lit input = (\map -> Just input)

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * 1) + 4"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- data ExprT = Lit Integer
--   | Add ExprT ExprT
--   | Mul ExprT ExprT
-- deriving (Show, Eq)