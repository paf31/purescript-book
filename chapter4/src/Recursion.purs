module Recursion where

import Data.Array
import Data.Foldable

squares :: [Number] -> [Number]
squares xs = (\n -> n * n) <$> xs

evens :: [Number] -> [Number]
evens xs = filter isEven xs
  where
  isEven :: Number -> Boolean
  isEven n = n % 2 == 0

infix 5 ..

(..) :: Number -> Number -> [Number]
(..) = range

factors :: Number -> [[Number]]
factors n = filter (\xs -> product xs == n) $
	    concatMap (\lo -> map (\hi -> [lo, hi]) (lo .. n)) (1 .. n)

factors' :: Number -> [[Number]]
factors' n = filter (\xs -> product xs == n) $ do
  lo <- 1 .. n
  hi <- lo .. n
  return [lo, hi]

average :: [Number] -> Number
average xs = 
  let pair = foldl (\o n -> { sum: o.sum + n, count: o.count + 1 }) { sum: 0, count: 0 } xs
  in pair.sum / pair.count
