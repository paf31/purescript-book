module Network.HTTP.Client where

import Prelude

import Types

import Data.Either
import Data.Function

import Control.Monad.Eff
import Control.Monad.Cont.Trans

foreign import data HTTP :: !

type URI = String

foreign import getImpl :: 
                 forall eff. Fn3 URI
                   (String -> Eff (http :: HTTP | eff) Unit) 
                   (String -> Eff (http :: HTTP | eff) Unit) 
                   (Eff (http :: HTTP | eff) Unit)

get :: forall eff. URI -> Async (http :: HTTP | eff) (Either String String)
get req = ContT $ \k -> runFn3 getImpl req (k <<< Right) (k <<< Left)
