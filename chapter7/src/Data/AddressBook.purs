module Data.AddressBook where

import Prelude

newtype Address = Address
  { street :: String
  , city   :: String
  , state  :: String
  }

address :: String -> String -> String -> Address
address street city state = Address { street, city, state }

data PhoneType
  = HomePhone
  | WorkPhone
  | CellPhone
  | OtherPhone

newtype PhoneNumber = PhoneNumber
  { "type" :: PhoneType
  , number :: String
  }

phoneNumber :: PhoneType -> String -> PhoneNumber
phoneNumber ty number = PhoneNumber
  { "type": ty
  , number: number
  }

newtype Person = Person
  { firstName   :: String
  , lastName    :: String
  , homeAddress :: Address
  , phones      :: Array PhoneNumber
  }

person :: String -> String -> Address -> Array PhoneNumber -> Person
person firstName lastName homeAddress phones =
  Person { firstName, lastName, homeAddress, phones }

examplePerson :: Person
examplePerson =
  person "John" "Smith"
         (address "123 Fake St." "FakeTown" "CA")
         [ phoneNumber HomePhone "555-555-5555"
         , phoneNumber CellPhone "555-555-0000"
         ]

instance showAddress :: Show Address where
  show (Address o) = "Address " <>
    "{ street: " <> show o.street <>
    ", city: "   <> show o.city <>
    ", state: "  <> show o.state <>
    " }"

instance showPhoneType :: Show PhoneType where
  show HomePhone = "HomePhone"
  show WorkPhone = "WorkPhone"
  show CellPhone = "CellPhone"
  show OtherPhone = "OtherPhone"

instance showPhoneNumber :: Show PhoneNumber where
  show (PhoneNumber o) = "PhoneNumber " <>
    "{ type: "   <> show o."type" <>
    ", number: " <> show o.number <>
    " }"

instance showPerson :: Show Person where
  show (Person o) = "Person " <>
    "{ firstName: "   <> show o.firstName <>
    ", lastName: "    <> show o.lastName <>
    ", homeAddress: " <> show o.homeAddress <>
    ", phones: "      <> show o.phones <>
    " }"
