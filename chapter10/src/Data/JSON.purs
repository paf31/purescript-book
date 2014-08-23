module Data.JSON where

foreign import stringify
  "function stringify(x) {\
  \  return JSON.stringify(x);\
  \}" :: forall a. a -> String

