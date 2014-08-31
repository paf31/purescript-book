module Main where

import Data.Maybe
import Data.Array (map)
import Data.Function (on)
import Data.String (joinWith, length)

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Cont.Trans

import Network.HTTP.Client

import Debug.Trace

main = flip runContT print do
  runParallel $ 
    longest <$> Parallel (getResponseText purescript_org)
            <*> Parallel (getResponseText try_purescript_org)
  where
  longest :: String -> String -> Ordering
  longest = compare `on` length

  getResponseText req = responseToString <$> getAll req

  responseToString :: Response -> String
  responseToString (Response chunks) = joinWith "" $ map runChunk chunks
  
  purescript_org :: Request
  purescript_org = Request 
    { host: "www.purescript.org"
    , path: "/" 
    }

  try_purescript_org :: Request
  try_purescript_org = Request
    { host: "try.purescript.org"
    , path: "/"
    }
