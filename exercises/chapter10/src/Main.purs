module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Alert (ALERT, alert)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Storage (STORAGE, setItem, getItem)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), PhoneType(..), phoneNumber, address, person, examplePerson)
import Data.AddressBook.Validation (Errors, validatePerson')
import Data.Array ((..), length, modifyAt, zipWith)
import Data.Either (Either(..))
import Data.Foldable (foldMap, for_)
import Data.Foreign (ForeignError, readNullOrUndefined, readString, renderForeignError, toForeign)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Foreign.Index (index)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReadWrite, ReactState, Event, ReactThis, createFactory, readState, spec, createClass, writeState)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)

newtype AppState = AppState
  { person :: Person
  , errors :: Errors
  }

initialState :: Maybe FormData -> AppState
initialState Nothing = AppState { person: examplePerson, errors: [] }
initialState (Just (FormData { firstName
                             , lastName
                             , street
                             , city
                             , state
                             , homePhone
                             , cellPhone
                             })) =
  AppState { person: person firstName lastName
                       (address street city state)
                       [ phoneNumber HomePhone homePhone
                       , phoneNumber CellPhone cellPhone
                       ]
           , errors: []
           }

newtype FormData = FormData
  { firstName  :: String
  , lastName   :: String
  , street     :: String
  , city       :: String
  , state      :: String
  , homePhone  :: String
  , cellPhone  :: String
  }

derive instance genericFormData :: Generic FormData _

instance decodeFormData :: Decode FormData where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

instance encodeFormData :: Encode FormData where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

toFormData :: Partial => Person -> FormData
toFormData (Person p@{ homeAddress: Address a
                     , phones: [ PhoneNumber pn1
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

loadSavedData
  :: forall eff
   . Eff ( console :: CONSOLE
         , alert :: ALERT
         , dom :: DOM
         , storage :: STORAGE
         | eff
         ) (Maybe FormData)
loadSavedData = do
  item <- getItem "person"

  let
    savedData :: Either (NonEmptyList ForeignError) (Maybe FormData)
    savedData = runExcept do
      jsonOrNull <- traverse readString =<< readNullOrUndefined item
      traverse decodeJSON jsonOrNull

  case savedData of
    Left err -> do
      alert $ "Unable to read saved form data: " <> foldMap (("\n" <> _) <<< renderForeignError) err
      pure Nothing
    Right mdata -> pure mdata

validateAndSaveEntry
  :: forall eff
   . Person
  -> Eff ( console :: CONSOLE
         , alert :: ALERT
         , dom :: DOM
         , storage :: STORAGE
         | eff
         ) Unit
validateAndSaveEntry person = do
  log "Running validators"
  case validatePerson' person of
    Left errs -> alert $ "There are " <> show (length errs) <> " validation errors."
    Right result -> do
      setItem "person" $ encodeJSON $ unsafePartial toFormData result
      alert "Saved"

valueOf :: Event -> Either (NonEmptyList ForeignError) String
valueOf e = runExcept do
  target <- index (toForeign e) "target"
  value <- index target "value"
  readString value

updateAppState
  :: forall props eff
   . ReactThis props AppState
  -> (String -> Person)
  -> Event
  -> Eff ( console :: CONSOLE
         , state :: ReactState ReadWrite
         | eff
         ) Unit
updateAppState ctx update e = do
  val <- readState ctx

  for_ (valueOf e) \s -> do
    let newPerson = update s

    log "Running validators"
    case validatePerson' newPerson of
      Left errors -> writeState ctx (AppState { person: newPerson, errors: errors })
      Right _ -> writeState ctx (AppState { person: newPerson, errors: [] })

addressBook :: forall props. AppState -> ReactClass props
addressBook state = createClass $ spec state \ctx -> do
  AppState { person: Person person@{ homeAddress: Address address }, errors } <- readState ctx

  let renderValidationError err = D.li' [ D.text err ]

      renderValidationErrors [] = []
      renderValidationErrors xs =
        [ D.div [ P.className "alert alert-danger" ]
                [ D.ul' (map renderValidationError xs) ]
        ]

      formField name hint value update =
        D.div [ P.className "form-group" ]
              [ D.label [ P.className "col-sm-2 control-label" ]
                        [ D.text name ]
              , D.div [ P.className "col-sm-3" ]
                      [ D.input [ P._type "text"
                                , P.className "form-control"
                                , P.placeholder hint
                                , P.value value
                                , P.onChange (updateAppState ctx update)
                                ] []
                      ]
              ]

      renderPhoneNumber (PhoneNumber phone) index =
        formField (show phone."type") "XXX-XXX-XXXX" phone.number \s ->
          Person $ person { phones = fromMaybe person.phones $ modifyAt index (updatePhoneNumber s) person.phones }

      updateFirstName s = Person $ person { firstName = s }
      updateLastName  s = Person $ person { lastName  = s }

      updateStreet s = Person $ person { homeAddress = Address $ address { street = s } }
      updateCity   s = Person $ person { homeAddress = Address $ address { city   = s } }
      updateState  s = Person $ person { homeAddress = Address $ address { state  = s } }

      updatePhoneNumber s (PhoneNumber o) = PhoneNumber $ o { number = s }

  pure $
    D.div [ P.className "container" ]
          [ D.div [ P.className "row" ]
                  (renderValidationErrors errors)
          , D.div [ P.className "row" ]
                  [ D.form [ P.className "form-horizontal" ] $
                           [ D.h3' [ D.text "Basic Information" ]

                           , formField "First Name" "First Name" person.firstName updateFirstName
                           , formField "Last Name"  "Last Name"  person.lastName  updateLastName

                           , D.h3' [ D.text "Address" ]

                           , formField "Street" "Street" address.street updateStreet
                           , formField "City"   "City"   address.city   updateCity
                           , formField "State"  "State"  address.state  updateState

                           , D.h3' [ D.text "Contact Information" ]
                           ]
                           <> zipWith renderPhoneNumber person.phones (0 .. length person.phones)
                           <> [ D.div [ P.className "form-group" ]
                                 [ D.label [ P.className "col-sm-3 col-sm-offset-2" ]
                                           [ D.button [ P.className "btn btn-primary"
                                                      , P.onClick \_ -> validateAndSaveEntry (Person person)
                                                      ]
                                                      [ D.text "Save" ]
                                           ]
                                 ]
                              ]
                  ]
          ]

main :: Eff ( alert :: ALERT
            , console :: CONSOLE
            , dom :: DOM
            , storage :: STORAGE
            ) Unit
main = void do
  log "Loading data from local storage"
  formData <- loadSavedData

  log "Rendering address book component"
  let component = D.div [] [ createFactory (addressBook (initialState formData)) unit ]
  doc <- window >>= document
  ctr <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  render component (unsafePartial fromJust ctr)
