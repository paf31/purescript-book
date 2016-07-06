module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Address(..), Person(..), PhoneNumber(..),
                         address, person, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, test, noFlags, regex)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

type Errors = Array String

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
nonEmpty _     _  = pure unit

arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid ["Field '" <> field <> "' must contain at least one value"]
arrayNonEmpty _     _  = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len = invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _     _   _     = pure unit

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial
    case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
      Right r -> r

matches :: String -> Regex -> String -> V Errors Unit
matches _     regex value | test regex value = pure unit
matches field _     _     = invalid ["Field '" <> field <> "' did not match the required format"]

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty "Street" o.street *> pure o.street)
          <*> (nonEmpty "City"   o.city   *> pure o.city)
          <*> (lengthIs "State" 2 o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty "First Name" o.firstName *> pure o.firstName)
         <*> (nonEmpty "Last Name"  o.lastName  *> pure o.lastName)
         <*> validateAddress o.homeAddress
         <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p
