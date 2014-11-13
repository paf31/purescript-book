module Main where

import Data.Array
import Data.Function (on)
import Data.Foldable (foldr)

import Merge
import Tree 

import Test.QuickCheck
import Test.QuickCheck.Gen

isSorted :: forall a. (Ord a) => [a] -> Boolean
isSorted (x1 : t@(x2 : xs)) = x1 <= x2 && isSorted t
isSorted _ = true

isSubarrayOf :: forall a. (Eq a) => [a] -> [a] -> Boolean
isSubarrayOf xs ys = xs `intersect` ys == xs

newtype Sorted a = Sorted [a]

sorted :: forall a. Sorted a -> [a]
sorted (Sorted xs) = xs

instance arbSorted :: (Arbitrary a, Ord a) => Arbitrary (Sorted a) where
  arbitrary = Sorted <<< sort <$> arbitrary

numbers :: [Number] -> [Number]
numbers = id

numberToBool :: (Number -> Boolean) -> Number -> Boolean
numberToBool = id

instance arbTree :: (Arbitrary a, Ord a) => Arbitrary (Tree a) where
  arbitrary = fromArray <<< sorted <$> arbitrary

instance coarbTree :: (CoArbitrary a) => CoArbitrary (Tree a) where
  coarbitrary Leaf = id
  coarbitrary (Branch l a r) = 
    coarbitrary l <<< 
    coarbitrary a <<< 
    coarbitrary r

treeOfNumber :: Tree Number -> Tree Number
treeOfNumber = id

main = do
  -- Tests for module 'Merge'

  quickCheck $ \xs ys -> isSorted $ merge (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> xs `isSubarrayOf` merge xs ys

  quickCheck $ \xs ys -> isSorted $ numbers $ mergePoly (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> numbers xs `isSubarrayOf` mergePoly xs ys
  
  quickCheck $ \xs ys f -> isSorted $ map f $ mergeWith (numberToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
  quickCheck $ \xs ys f -> xs `isSubarrayOf` mergeWith (numberToBool f) xs ys
  
  -- Tests for module 'Tree'

  quickCheck $ \t a -> member a $ insert a (t :: Tree Number) 
  quickCheck $ \t xs -> isSorted $ toArray $ foldr insert t $ numbers xs

  quickCheck $ \f g t -> 
    anywhere (\s -> f s || g s) t == 
      anywhere f (treeOfNumber t) || anywhere g t
