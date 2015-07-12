module Test.Main where

import Prelude

import Data.List (List(..), toList)
import Data.Array (intersect, sort, sortBy)
import Data.Function (on)
import Data.Foldable (foldr)

import Sorted
import Merge
import Tree 

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

isSorted :: forall a. (Ord a) => Array a -> Boolean
isSorted = go <<< toList
  where
  go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t
  go _ = true

isSubarrayOf :: forall a. (Eq a) => Array a -> Array a -> Boolean
isSubarrayOf xs ys = xs `intersect` ys == xs

ints :: Array Int -> Array Int
ints = id

intToBool :: (Int -> Boolean) -> Int -> Boolean
intToBool = id

treeOfInt :: Tree Number -> Tree Number
treeOfInt = id

main = do
  -- Tests for module 'Merge'

  quickCheck $ \xs ys -> isSorted $ merge (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> xs `isSubarrayOf` merge xs ys

  quickCheck $ \xs ys -> isSorted $ ints $ mergePoly (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> ints xs `isSubarrayOf` mergePoly xs ys
  
  quickCheck $ \xs ys f -> isSorted $ map f $ mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
  quickCheck $ \xs ys f -> xs `isSubarrayOf` mergeWith (intToBool f) xs ys
  
  -- Tests for module 'Tree'

  quickCheck $ \t a -> member a $ insert a $ treeOfInt t 
  quickCheck $ \t xs -> isSorted $ toArray $ foldr insert t $ ints xs

  quickCheck $ \f g t -> 
    anywhere (\s -> f s || g s) t == 
      anywhere f (treeOfInt t) || anywhere g t
