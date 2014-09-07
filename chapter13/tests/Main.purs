module Main where

import Data.Array
import Data.Function (on)
import Data.Foldable (foldr)

import Merge
import Tree 

import Test.QuickCheck
import Test.QuickCheck.LCG

isSorted :: forall a. (Ord a) => [a] -> Boolean
isSorted (x1 : t@(x2 : xs)) = x1 <= x2 && isSorted t
isSorted _ = true

isSubarrayOf :: forall a. (Eq a) => [a] -> [a] -> Boolean
isSubarrayOf xs ys = ys `intersect` xs == xs

numbers :: [Number] -> [Number]
numbers = id

numberToBool :: (Number -> Boolean) -> Number -> Boolean
numberToBool = id

instance arbTree :: (Arbitrary a, Ord a) => Arbitrary (Tree a) where
  arbitrary = fromArray <<< sort <$> arbitrary

main = do
  -- Tests for module 'Merge'

  quickCheck $ \xs ys -> isSorted $ merge (sort xs) (sort ys)
  quickCheck $ \xs ys -> xs `isSubarrayOf` merge xs ys

  quickCheck $ \xs ys -> isSorted $ numbers $ mergePoly (sort xs) (sort ys)
  quickCheck $ \xs ys -> numbers xs `isSubarrayOf` mergePoly xs ys
  
  quickCheck $ \xs ys f -> isSorted $ map f $ mergeWith (numberToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
  quickCheck $ \xs ys f -> xs `isSubarrayOf` mergeWith (numberToBool f) xs ys
  
  -- Tests for module 'Tree'

  quickCheck $ \t a -> member a $ insert a (t :: Tree Number) 
  quickCheck $ \xs -> isSorted $ toArray $ foldr insert Leaf $ numbers xs
