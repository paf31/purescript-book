module Data.AddressBook.UI where

import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.AddressBook
import Data.AddressBook.Validation

import Data.Traversable

import Control.Bind

import Control.Monad.Eff
import Control.Monad.Eff.DOM

import Debug.Trace

valueOf :: forall eff. String -> Eff (dom :: DOM | eff) String
valueOf sel = do
  maybeEl <- querySelector sel
  case maybeEl of
    Nothing -> return ""
    Just el -> do
      value <- getValue el
      return $ case read value of
        Right s -> s
        _ -> ""

displayValidationErrors :: forall eff. [String] -> Eff (dom :: DOM | eff) Unit
displayValidationErrors errs = do
  alert <- createElement "div"
    >>= addClass "alert"
    >>= addClass "alert-danger"

  ul <- createElement "ul"
  ul `appendChild` alert

  foreachE errs $ \err -> do
    li <- createElement "li" >>= setText err
    li `appendChild` ul
    return unit

  Just validationErrors <- querySelector "#validationErrors"
  alert `appendChild` validationErrors

  return unit

validateControls :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) (Either [String] Person)
validateControls = do
  trace "Running validators"

  p <- person <$> valueOf "#inputFirstName"
              <*> valueOf "#inputLastName"
              <*> (address <$> valueOf "#inputStreet"
                           <*> valueOf "#inputCity"
                           <*> valueOf "#inputState")
              <*> sequence [ phoneNumber HomePhone <$> valueOf "#inputHomePhone"
                           , phoneNumber CellPhone <$> valueOf "#inputCellPhone"
                           ]

  return $ validatePerson' p

validateAndUpdateUI :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) Unit
validateAndUpdateUI = do
  Just validationErrors <- querySelector "#validationErrors"
  setInnerHTML "" validationErrors

  errorsOrResult <- validateControls

  case errorsOrResult of
    Left errs -> displayValidationErrors errs
    Right result -> print result

  return unit

setupEventHandlers :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) Unit
setupEventHandlers = do
  -- Listen for changes on form fields
  body >>= addEventListener "change" validateAndUpdateUI

  return unit
