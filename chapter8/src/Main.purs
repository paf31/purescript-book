module Main where

import Data.Array

import Control.Monad.Eff
import Control.Monad.Eff.DOM (DOM())

import Debug.Trace

import Data.AddressBook.UI

main :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) Unit
main = do
  trace "Attaching event handlers"
  setupEventHandlers 
