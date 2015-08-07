module Control.Monad.Eff.DOM where

import Prelude

import DOM

import Data.Maybe
import Data.Foreign
import Data.Function

import Control.Monad.Eff

foreign import body :: forall eff. Eff (dom :: DOM | eff) Node

foreign import createElement :: forall eff. String -> Eff (dom :: DOM | eff) Node 

foreign import querySelectorImpl :: forall eff r. Fn3 r (Node -> r) String (Eff (dom :: DOM | eff) r)

querySelector :: forall eff. String -> Eff (dom :: DOM | eff) (Maybe Node)
querySelector s = runFn3 querySelectorImpl Nothing Just s

foreign import appendChild :: forall eff. Node -> Node -> Eff (dom :: DOM | eff) Node

foreign import addClass :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node

foreign import setText :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node 

foreign import getValue :: forall eff. Node -> Eff (dom :: DOM | eff) Foreign

foreign import setValue :: forall a eff. a -> Node -> Eff (dom :: DOM | eff) Node 

foreign import setInnerHTML :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node

foreign import addEventListener :: forall eff. String -> Eff (dom :: DOM | eff) Unit -> Node -> Eff (dom :: DOM | eff) Unit 
