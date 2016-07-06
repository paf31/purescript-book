module Sorted where

import Prelude

import Data.Array (sort)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

newtype Sorted a = Sorted (Array a)

sorted :: forall a. Sorted a -> Array a
sorted (Sorted xs) = xs

instance showSorted :: Show a => Show (Sorted a) where
  show = show <<< sorted

instance arbSorted :: (Arbitrary a, Ord a) => Arbitrary (Sorted a) where
  arbitrary = map (Sorted <<< sort) arbitrary
