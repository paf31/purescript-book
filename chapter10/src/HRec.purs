module HRec where

import Data.Function

foreign import data HRec :: * -> *

instance showHRec :: (Show a) => Show (HRec a) where
  show rec = "HRec { " ++ Data.String.joinWith ", " (runFn3 foldHRec f [] rec) ++ " }"
    where
    f = mkFn3 $ \ss k a -> (show k ++ ": " ++ show a) : ss

foreign import empty 
  "var empty = {}" :: forall a. HRec a

foreign import insert
  "function insert(key, value, rec) {\
  \  var copy = {};\
  \  for (var k in rec) {\
  \    if (rec.hasOwnProperty(k)) {\
  \      copy[k] = rec[k];\
  \    }\
  \  }\
  \  copy[key] = value;\
  \  return copy;\
  \}" :: forall a. Fn3 String a (HRec a) (HRec a)

foreign import mapHRec
  "function mapHRec(f, rec) {\
  \  var mapped = {};\
  \  for (var k in rec) {\
  \    if (rec.hasOwnProperty(k)) {\
  \      mapped[k] = f(rec[k]);\
  \    }\
  \  }\
  \  return mapped;\
  \}" :: forall a b. Fn2 (a -> b) (HRec a) (HRec b)

instance functorHRec :: Functor HRec where
  (<$>) f rec = runFn2 mapHRec f rec

foreign import foldHRec
  "function foldHRec(f, r, rec) {\
  \  var acc = r;\
  \  for (var k in rec) {\
  \    if (rec.hasOwnProperty(k)) {\
  \      acc = f(acc, k, rec[k]);\
  \    }\
  \  }\
  \  return acc;\
  \}" :: forall a r. Fn3 (Fn3 r String a r) r (HRec a) r

