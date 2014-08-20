module Main where

import Data.Maybe
import Data.Array (length)
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.JSON
import Data.AddressBook
import Data.AddressBook.UI

import Control.Monad.Eff
import Control.Monad.Eff.DOM
import Control.Monad.Eff.Storage

import Debug.Trace

foreign import data Alert :: !

foreign import alert
  "function alert(msg) {\
  \  return function() {\
  \    window.alert(msg);\
  \    return {};\
  \  };\
  \}" :: forall eff. String -> Eff (alert :: Alert | eff) Unit

newtype FormData = FormData
  { firstName  :: String
  , lastName   :: String

  , street     :: String
  , city       :: String
  , state      :: String

  , homePhone  :: String
  , cellPhone  :: String
  }

instance readForeignFormData :: IsForeign FormData where
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

loadSavedData :: forall eff. Eff (trace :: Trace, alert :: Alert, dom :: DOM, storage :: Storage | eff) Unit
loadSavedData = do
  json <- getItem "person"

  case readJSON json of
    Left _ -> return unit
    Right (FormData o) -> do
      updateForm "#inputFirstName" o.firstName
      updateForm "#inputLastName"  o.lastName

      updateForm "#inputStreet"    o.street
      updateForm "#inputCity"      o.city
      updateForm "#inputState"     o.state
      
      updateForm "#inputHomePhone" o.homePhone
      updateForm "#inputCellPhone" o.cellPhone
      
      return unit

validateAndSaveEntry :: forall eff. Eff (trace :: Trace, alert :: Alert, dom :: DOM, storage :: Storage | eff) Unit
validateAndSaveEntry = do
  trace "Running validators"
  
  errorsOrResult <- validateControls

  case errorsOrResult of
    Left errs -> alert $ "There are " ++ show (length errs) ++ " validation errors."
    Right result -> do
      setItem "person" (stringify (toFormData result))
      alert "Saved"

  return unit

main :: forall eff. Eff (trace :: Trace, alert :: Main.Alert, dom :: DOM, storage :: Storage | eff) Unit
main = do
  trace "Loading data from local storage"
  loadSavedData

  trace "Attaching event handlers"
  setupEventHandlers

  Just saveButton <- querySelector "#saveButton"
  
  addEventListener "click" validateAndSaveEntry saveButton
 
  return unit 
