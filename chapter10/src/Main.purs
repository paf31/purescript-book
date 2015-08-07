module Main where

import Prelude

import Data.Maybe
import Data.Array (length)
import Data.Either
import Data.Foreign
import Data.Foreign.Null
import Data.Foreign.Class
import Data.JSON
import Data.Traversable
import Data.AddressBook
import Data.AddressBook.UI

import DOM

import Control.Monad.Eff
import Control.Monad.Eff.DOM
import Control.Monad.Eff.Alert
import Control.Monad.Eff.Storage
import Control.Monad.Eff.Console

newtype FormData = FormData
  { firstName  :: String
  , lastName   :: String

  , street     :: String
  , city       :: String
  , state      :: String

  , homePhone  :: String
  , cellPhone  :: String
  }

instance formDataIsForeign :: IsForeign FormData where
  read value = do
    firstName   <- readProp "firstName" value
    lastName    <- readProp "lastName"  value

    street      <- readProp "street"    value
    city        <- readProp "city"      value
    state       <- readProp "state"     value

    homePhone   <- readProp "homePhone" value
    cellPhone   <- readProp "cellPhone" value

    return $ FormData
      { firstName  : firstName
      , lastName   : lastName

      , street     : street
      , city       : city
      , state      : state

      , homePhone  : homePhone
      , cellPhone  : cellPhone
      }

toFormData :: Person -> FormData
toFormData (Person p@{ address = Address a
                     , phones = [ PhoneNumber pn1
                                , PhoneNumber pn2
                                ]
                     }) =
  FormData { firstName  : p.firstName
           , lastName   : p.lastName

           , street     : a.street
           , city       : a.city
           , state      : a.state

           , homePhone  : pn1.number
           , cellPhone  : pn2.number
           }


updateForm :: forall eff. String -> String -> Eff (dom :: DOM | eff) Unit
updateForm sel value = do
  Just element <- querySelector sel
  setValue value element
  return unit

loadSavedData :: forall eff. Eff (console :: CONSOLE, alert :: ALERT, dom :: DOM, storage :: STORAGE | eff) Unit
loadSavedData = do
  item <- getItem "person"

  let
    savedData :: F (Maybe FormData)
    savedData = do
      jsonOrNull <- read item
      traverse readJSON (runNull jsonOrNull)

  case savedData of
    Left err -> alert $ "Unable to read saved form data: " ++ show err
    Right Nothing -> return unit
    Right (Just (FormData o)) -> do
      updateForm "#inputFirstName" o.firstName
      updateForm "#inputLastName"  o.lastName

      updateForm "#inputStreet"    o.street
      updateForm "#inputCity"      o.city
      updateForm "#inputState"     o.state

      updateForm "#inputHomePhone" o.homePhone
      updateForm "#inputCellPhone" o.cellPhone

      return unit

validateAndSaveEntry :: forall eff. Eff (console :: CONSOLE, alert :: ALERT, dom :: DOM, storage :: STORAGE | eff) Unit
validateAndSaveEntry = do
  log "Running validators"

  errorsOrResult <- validateControls

  case errorsOrResult of
    Left errs -> alert $ "There are " ++ show (length errs) ++ " validation errors."
    Right result -> do
      setItem "person" $ stringify $ toForeign $ toFormData result
      alert "Saved"

  return unit

main :: forall eff. Eff (console :: CONSOLE, alert :: ALERT, dom :: DOM, storage :: STORAGE | eff) Unit
main = do
  log "Loading data from local storage"
  loadSavedData

  log "Attaching event handlers"
  setupEventHandlers

  Just saveButton <- querySelector "#saveButton"

  addEventListener "click" validateAndSaveEntry saveButton

  return unit
