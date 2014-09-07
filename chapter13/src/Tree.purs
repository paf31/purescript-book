module Tree where

import Data.Array ()

import Test.QuickCheck
import Test.QuickCheck.LCG

data Tree a 
  = Leaf
  | Branch (Tree a) a (Tree a)

instance arbTree :: (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    b <- arbitrary
    if b then pure Leaf
         else Branch <$> arbitrary <*> arbitrary <*> arbitrary

insert :: forall a. (Ord a) => a -> Tree a -> Tree a
insert a Leaf = Branch Leaf a Leaf
insert a (Branch l a1 r) | a < a1 = Branch (insert a l) a1 r
insert a (Branch l a1 r) = Branch l a1 (insert a r)

toArray :: forall a. Tree a -> [a]
toArray Leaf = []
toArray (Branch l a r) = toArray l ++ [a] ++ toArray r

