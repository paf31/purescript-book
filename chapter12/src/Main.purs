module Main where

import Prelude

import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Monad.Trans (lift)
import Data.Either (either)
import Network.HTTP.Client (HTTP, get)
import Types (Async)

main :: Eff ( http :: HTTP
            , console :: CONSOLE
            ) Unit
main = async do
    response <- get "http://purescript.org"
    lift (either error log response)
  where
    async :: forall eff. Async eff Unit -> Eff eff Unit
    async = flip runContT pure
