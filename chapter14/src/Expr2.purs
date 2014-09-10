module Expr2 where

type N = Number

type B = Boolean

class Expr t where
  num      ::          N -> t N
  bool     ::          B -> t B
  add      :: t N -> t N -> t N
  multiply :: t N -> t N -> t N
  lt       :: t N -> t N -> t B
  ite      :: forall a. t B -> t a -> t a -> t a

type Typed a = forall t. (Expr t) => t a

newtype Eval a = Eval a

eval :: forall a. Eval a -> a
eval (Eval a) = a

instance exprEval :: Expr Eval where
  num n = Eval n
  bool b = Eval b
  add (Eval n) (Eval m) = Eval (n + m)
  multiply (Eval n) (Eval m) = Eval (n * m)
  lt (Eval n) (Eval m) = Eval (n < m)
  ite (Eval true) t _ = t
  ite (Eval false) _ f = f
