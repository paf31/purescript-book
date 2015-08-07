module Data.DOM.Phantom
  ( Element()
  , Attribute()
  , Content()
  , AttributeKey()
  , IsValue
  , toValue

  , a
  , p
  , img 

  , href
  , _class
  , src
  , width
  , height

  , (:=)
  , text
  , elem
  
  , render
  ) where

import Prelude

import Data.Maybe
import Data.String (joinWith)

newtype Element = Element
  { name         :: String
  , attribs      :: Array Attribute
  , content      :: Maybe (Array Content)
  }

data Content 
  = TextContent String
  | ElementContent Element

newtype Attribute = Attribute
  { key          :: String 
  , value        :: String
  }

newtype AttributeKey a = AttributeKey String

element :: String -> Array Attribute -> Maybe (Array Content) -> Element
element name attribs content = Element
  { name:      name
  , attribs:   attribs
  , content:   content
  }

text :: String -> Content
text = TextContent

elem :: Element -> Content
elem = ElementContent

class IsValue a where
  toValue :: a -> String

instance stringIsValue :: IsValue String where
  toValue = id
 
instance intIsValue :: IsValue Int where
  toValue = show
 
instance numberIsValue :: IsValue Number where
  toValue = show

(:=) :: forall a. (IsValue a) => AttributeKey a -> a -> Attribute
(:=) (AttributeKey key) value = Attribute
  { key: key
  , value: toValue value
  }

a :: Array Attribute -> Array Content -> Element
a attribs content = element "a" attribs (Just content)

p :: Array Attribute -> Array Content -> Element
p attribs content = element "p" attribs (Just content)

img :: Array Attribute -> Element
img attribs = element "img" attribs Nothing

href :: AttributeKey String
href = AttributeKey "href"

_class :: AttributeKey String
_class = AttributeKey "class"

src :: AttributeKey String
src = AttributeKey "src"

width :: AttributeKey Number
width = AttributeKey "width"

height :: AttributeKey Number
height = AttributeKey "height"

render :: Element -> String
render (Element e) = 
  "<" ++ e.name ++
  " " ++ joinWith " " (map renderAttribute e.attribs) ++
  renderContent e.content

  where
  renderAttribute :: Attribute -> String
  renderAttribute (Attribute a) = a.key ++ "=\"" ++ a.value ++ "\""
  
  renderContent :: Maybe (Array Content) -> String
  renderContent Nothing = " />"
  renderContent (Just content) = 
    ">" ++ joinWith "" (map renderContentItem content) ++
    "</" ++ e.name ++ ">"
    where
    renderContentItem :: Content -> String
    renderContentItem (TextContent s) = s
    renderContentItem (ElementContent e) = render e

