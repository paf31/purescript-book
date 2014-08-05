module Data.AddressBook.UI where

import Data.Either
import Data.Foreign
import Data.AddressBook
import Data.AddressBook.Validation

import Data.Traversable

import Control.Bind

import Control.Monad.Eff
import Control.Monad.JQuery

import Debug.Trace

valueOf :: forall eff. String -> Eff (dom :: DOM | eff) String
valueOf sel = do
  e <- select sel
  f <- getValue e
  return $ case parseForeign read f of
    Right s -> s
    _ -> ""

displayValidationErrors :: forall eff. [String] -> Eff (dom :: DOM | eff) Unit
displayValidationErrors errs = do
  alert <- create "<div>"
    >>= addClass "alert" 
    >>= addClass "alert-danger"

  ul <- create "<ul>"
  ul `append` alert

  for errs $ \err -> do
    li <- create "<li>" >>= setText err
    li `append` ul
    return unit
  
  select "#validationErrors" >>= clear >>= append alert
  
  return unit

validateControls :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) Unit
validateControls = do
  trace "Running validators"
  
  p <- person <$> valueOf "#inputFirstName"
              <*> valueOf "#inputLastName"
              <*> (address <$> valueOf "#inputStreet"
                           <*> valueOf "#inputCity"
                           <*> valueOf "#inputState")
              <*> pure [phoneNumber HomePhone "555-555-5555"]
              
  case validatePerson' p of
    Left errs -> displayValidationErrors errs
    Right result -> do
      print result
  
fromTemplate :: forall eff. String -> Eff (dom :: DOM | eff) JQuery
fromTemplate = select >=> getText >=> create
  
createPhoneNumberElement :: forall eff. JQueryEvent -> Eff (trace :: Trace, dom :: DOM | eff) Unit
createPhoneNumberElement e = do
  trace "Creating phone number element"
  
  -- Look up the HTML template by ID
  newElement <- fromTemplate "#phoneNumberTemplate" 
  
  -- Append the new element to the form
  select "#phoneNumbers" >>= append newElement
  
  -- Prevent navigation
  preventDefault e

setupEventHandlers :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) Unit
setupEventHandlers = do
  -- Select the body element
  body <- select "body"
  
  -- Listen for changes on form fields
  on' "change" ".input" (\_ _ -> validateControls) body
  
  -- Listen for clicks on the Add Phone Number button
  addPhoneNumber <- select "#addPhoneNumber"
  on "click" (\e _ -> createPhoneNumberElement e) addPhoneNumber
  
  return unit