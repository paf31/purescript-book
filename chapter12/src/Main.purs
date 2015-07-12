module Main where
    
import Prelude

import Types

import Data.Either

import Control.Monad.Eff
import Control.Monad.Eff.Console (log, error)
import Control.Monad.Trans
import Control.Monad.Cont.Trans

import Network.HTTP.Client

main = async do
  response <- get "http://purescript.org"
  lift (either error log response)
  where
  async :: forall eff. Async eff Unit -> Eff eff Unit
  async = flip runContT return
