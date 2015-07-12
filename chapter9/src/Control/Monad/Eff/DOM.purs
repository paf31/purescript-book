module Control.Monad.Eff.DOM where

import Prelude

import DOM

import Data.Maybe
import Data.Function

import Control.Monad.Eff

foreign import querySelectorImpl :: forall eff r. Fn3 r (Node -> r) String (Eff (dom :: DOM | eff) r)

querySelector :: forall eff. String -> Eff (dom :: DOM | eff) (Maybe Node)
querySelector s = runFn3 querySelectorImpl Nothing Just s

foreign import addEventListener :: forall eff. String -> Eff (dom :: DOM | eff) Unit -> Node -> Eff (dom :: DOM | eff) Unit 
