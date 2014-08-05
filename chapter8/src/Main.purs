module Main where

import Data.Array

import Control.Monad.Eff
import Debug.Trace

import Data.AddressBook.UI

main = do
  trace "Attaching event handlers"
  setupEventHandlers 
