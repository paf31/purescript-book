module Main where

import Data.Array
import Data.Function (on)
import Data.Foldable (foldr)

import Merge
import Tree 

import Test.QuickCheck

isSorted :: forall a. (Ord a) => [a] -> Boolean
isSorted (x1 : t@(x2 : xs)) = x1 <= x2 && isSorted t
isSorted _ = true

isSubsetOf :: forall a. (Eq a) => [a] -> [a] -> Boolean
isSubsetOf xs ys = xs `intersect` ys == xs

numbers :: [Number] -> [Number]
numbers = id

numberToBool :: (Number -> Boolean) -> Number -> Boolean
numberToBool = id

main = do
  -- Tests for module 'Merge'

  quickCheck $ \xs ys -> isSorted $ merge (sort xs) (sort ys)
  quickCheck $ \xs ys -> xs `isSubsetOf` merge xs ys

  quickCheck $ \xs ys -> isSorted $ numbers $ mergePoly (sort xs) (sort ys)
  quickCheck $ \xs ys -> numbers xs `isSubsetOf` mergePoly xs ys
  
  quickCheck $ \xs ys f -> isSorted $ map f $ mergeWith (numberToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
  quickCheck $ \xs ys f -> xs `isSubsetOf` mergeWith (numberToBool f) xs ys
  
  -- Tests for module 'Tree'

  quickCheck $ \xs -> isSorted $ toArray $ foldr insert Leaf $ numbers xs
