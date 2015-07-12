module Test.Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Maybe
import Data.AddressBook

example :: Entry
example = 
  { firstName: "John"
  , lastName: "Smith"
  , address: { street: "123 Fake St."
             , city: "Faketown"
             , state: "CA"
             }
  }
     
book0 :: AddressBook
book0 = emptyBook

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book

main = do
  let book1 = insertEntry example emptyBook

  print $ printEntry "John" "Smith" book0
  print $ printEntry "John" "Smith" book1
