module Data.JSON where

import Data.Foreign

foreign import stringify
  "function stringify(x) {\
  \  return JSON.stringify(x);\
  \}" :: forall a. a -> String

foreign import parse
  "function parse(s) {\
  \  return JSON.parse(s);\
  \}" :: String -> Foreign
