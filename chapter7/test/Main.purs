module Test.Main where

import Data.AddressBook
import Data.AddressBook.Validation

import Control.Monad.Eff.Console

main = print (validatePerson examplePerson)
