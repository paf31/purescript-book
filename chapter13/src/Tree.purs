module Tree where

import Prelude

import Data.Foldable (foldr)
import Test.QuickCheck.Arbitrary (class Coarbitrary, class Arbitrary, coarbitrary, arbitrary)

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)

instance arbTree :: (Arbitrary a, Ord a) => Arbitrary (Tree a) where
  arbitrary = map fromArray arbitrary

instance coarbTree :: (Coarbitrary a) => Coarbitrary (Tree a) where
  coarbitrary Leaf = id
  coarbitrary (Branch l a r) =
    coarbitrary l <<<
    coarbitrary a <<<
    coarbitrary r

insert :: forall a. (Ord a) => a -> Tree a -> Tree a
insert a Leaf = Branch Leaf a Leaf
insert a (Branch l a1 r) | a < a1 = Branch (insert a l) a1 r
insert a (Branch l a1 r) = Branch l a1 (insert a r)

member :: forall a. (Ord a) => a -> Tree a -> Boolean
member _ Leaf = false
member a (Branch _ a1 _) | a == a1 = true
member a (Branch l a1 _) | a < a1 = a `member` l
member a (Branch _ _  r) = a `member` r

toArray :: forall a. Tree a -> Array a
toArray Leaf = []
toArray (Branch l a r) = toArray l <> [a] <> toArray r

fromArray :: forall a. (Ord a) => Array a -> Tree a
fromArray = foldr insert Leaf

anywhere :: forall a. (Tree a -> Boolean) -> Tree a -> Boolean
anywhere f Leaf = f Leaf
anywhere f t@(Branch l _ r) = anywhere f l || f t || anywhere f r
