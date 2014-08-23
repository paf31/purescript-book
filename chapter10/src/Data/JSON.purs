module Data.JSON where

import Data.Foreign (Foreign())

foreign import stringify
  "function stringify(x) {\
  \  return JSON.stringify(x);\
  \}" :: Foreign -> String

