module Main where

import Data.Array (map)
import Data.String (joinWith, length)

import Control.Monad.Eff
import Control.Monad.Cont.Trans

import Network.HTTP.Client

import Debug.Trace

main = runContT (getResponseText purescript_org) trace
  where
  getResponseText req = responseToString <$> getAll req

  responseToString :: Response -> String
  responseToString (Response chunks) = joinWith "" $ map runChunk chunks
  
  purescript_org :: Request
  purescript_org = Request 
    { host: "www.purescript.org"
    , path: "/" 
    }
