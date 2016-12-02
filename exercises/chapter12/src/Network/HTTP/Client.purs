module Network.HTTP.Client where

import Prelude

import Control.Monad.Cont.Trans (ContT(..))
import Control.Monad.Eff (Eff)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Types (Async)

foreign import data HTTP :: !

type URI = String

foreign import getImpl ::
                 forall eff. Fn3 URI
                   (String -> Eff (http :: HTTP | eff) Unit)
                   (String -> Eff (http :: HTTP | eff) Unit)
                   (Eff (http :: HTTP | eff) Unit)

get :: forall eff. URI -> Async (http :: HTTP | eff) (Either String String)
get req = ContT $ \k -> runFn3 getImpl req (k <<< Right) (k <<< Left)
