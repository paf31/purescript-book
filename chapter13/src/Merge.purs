module Merge where

merge :: [Number] -> [Number] -> [Number]
merge = mergePoly

mergePoly :: forall a. (Ord a) => [a] -> [a] -> [a]
mergePoly = mergeWith id

mergeWith :: forall a b. (Ord b) => (a -> b) -> [a] -> [a] -> [a]
mergeWith f = go
  where
  go [] ys = ys
  go xs [] = xs
  go xs@(x : xs') ys@(y : ys') = 
    case compare (f x) (f y) of 
      LT -> x : go xs' ys
      _  -> y : go xs  ys'
