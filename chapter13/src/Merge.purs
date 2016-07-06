module Merge where

import Prelude

import Data.List (List(..), fromFoldable, toUnfoldable, reverse)

merge :: Array Int -> Array Int -> Array Int
merge = mergePoly

mergePoly :: forall a. Ord a => Array a -> Array a -> Array a
mergePoly = mergeWith id

mergeWith :: forall a b. Ord b => (a -> b) -> Array a -> Array a -> Array a
mergeWith f xs ys = toUnfoldable (go Nil (fromFoldable xs) (fromFoldable ys))
  where
  go acc Nil ys  = reverse acc <> ys
  go acc xs  Nil = reverse acc <> xs
  go acc xs@(Cons x xs') ys@(Cons y ys') =
    case compare (f x) (f y) of
      LT -> go (Cons x acc) xs' ys
      _  -> go (Cons y acc) xs  ys'
