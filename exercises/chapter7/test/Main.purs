module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.AddressBook (examplePerson)
import Data.AddressBook.Validation (validatePerson)

main :: Eff (console :: CONSOLE) Unit
main = logShow (validatePerson examplePerson)
