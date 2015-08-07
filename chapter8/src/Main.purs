module Main where

import Prelude

import DOM

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.AddressBook.UI

main :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
main = do
  log "Attaching event handlers"
  setupEventHandlers 
