module Control.Monad.Eff.Alert where

import Control.Monad.Eff

foreign import data Alert :: !

foreign import alert
  "function alert(msg) {\
  \  return function() {\
  \    window.alert(msg);\
  \    return {};\
  \  };\
  \}" :: forall eff. String -> Eff (alert :: Alert | eff) Unit
