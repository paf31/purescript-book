module Control.Monad.Eff.Storage where

import Data.Foreign

import Control.Monad.Eff

foreign import data Storage :: !

foreign import setItem
  "function setItem(key) {\
  \  return function(value) {\
  \    return function() {\
  \      window.localStorage.setItem(key, value);\
  \    };\
  \  };\
  \}" :: forall eff. String -> String -> Eff (storage :: Storage | eff) Unit

foreign import getItem 
  "function getItem(key) {\
  \  return function() {\
  \    return window.localStorage.getItem(key);\
  \  }\
  \}" :: forall eff. String -> Eff (storage :: Storage | eff) Foreign 
