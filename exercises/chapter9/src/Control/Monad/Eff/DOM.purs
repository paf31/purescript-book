module Control.Monad.Eff.DOM where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import DOM (DOM)

foreign import data Node :: *

foreign import querySelectorImpl
  :: forall eff r
   . Fn3 r
         (Node -> r)
         String
         (Eff (dom :: DOM | eff) r)

querySelector
  :: forall eff
   . String
  -> Eff (dom :: DOM | eff) (Maybe Node)
querySelector s = runFn3 querySelectorImpl Nothing Just s

foreign import addEventListener
  :: forall eff
   . String
  -> Eff (dom :: DOM | eff) Unit
  -> Node
  -> Eff (dom :: DOM | eff) Unit
