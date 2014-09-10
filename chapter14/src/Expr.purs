module Expr 
  ( Typed()
  , num
  , bool
  , add
  , multiply
  , lt
  , ite
  , eval
  ) where

import Data.Either

data Typed a = Typed Untyped

data Untyped
  = Num Number
  | Bool Boolean
  | Add Untyped Untyped
  | Multiply Untyped Untyped
  | LessThan Untyped Untyped
  | IfThenElse Untyped Untyped Untyped

num :: Number -> Typed Number
num n = Typed $ Num n

bool :: Boolean -> Typed Boolean
bool b = Typed $ Bool b

add :: Typed Number -> Typed Number -> Typed Number
add (Typed n) (Typed m) = Typed $ Add n m

multiply :: Typed Number -> Typed Number -> Typed Number
multiply (Typed n) (Typed m) = Typed $ Multiply n m

lt :: Typed Number -> Typed Number -> Typed Boolean
lt (Typed n) (Typed m) = Typed $ LessThan n m

ite :: forall a. Typed Boolean -> Typed a -> Typed a -> Typed a
ite (Typed b) (Typed x) (Typed y) = Typed $ IfThenElse b x y

eval :: forall a. Typed a -> Either Number Boolean
eval (Typed untyped) = go untyped
  where
  go :: Untyped -> Either Number Boolean
  go (Num n) = Left n
  go (Bool b) = Right b
  go (Add n m) = 
    case [go n, go n] of
      [Left x, Left y] -> Left $ x + y
  go (Multiply n m) = 
    case [go n, go m] of
      [Left x, Left y] -> Left $ x * y
  go (LessThan n m) = 
    case [go n, go m] of
      [Left x, Left y] -> Right $ x < y
  go (IfThenElse b t f) = 
    case go b of
      Right true  -> go t
      Right false -> go f

