module Main where

import Data.Array (map)
import Data.Maybe
import Data.String (joinWith)

import Control.Monad.Eff
import Control.Monad.Cont.Trans

import Network.HTTP.Client

import Debug.Trace

main = runContT (getAll $ Request { host: "www.purescript.org", path: "/" }) $ \(Response chunks) -> do
  let html = joinWith "" $ map runChunk chunks
  trace html
