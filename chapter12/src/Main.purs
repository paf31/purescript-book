module Main where

import Data.Maybe

import Control.Monad.Eff
import Control.Monad.Cont.Trans

import Network.HTTP.Client

main = runContT (getCont $ Request { host: "www.purescript.org", path: "/" })
                Debug.Trace.print
