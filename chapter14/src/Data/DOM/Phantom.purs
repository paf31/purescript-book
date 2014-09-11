module Data.DOM.Phantom
  ( Element()
  , Attribute()
  , Content()
  , AttributeKey()
  , IsValue

  , a
  , div
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

import Data.Maybe
import Data.Array (map)
import Data.String (joinWith)

newtype Element = Element
  { name         :: String
  , attribs      :: [Attribute]
  , content      :: Maybe [Content]
  }

data Content 
  = TextContent String
  | ElementContent Element

newtype Attribute = Attribute
  { key          :: String 
  , value        :: String
  }

newtype AttributeKey a = AttributeKey String

element :: String -> [Attribute] -> Maybe [Content] -> Element
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

instance numberIsValue :: IsValue Number where
  toValue = show

(:=) :: forall a. (IsValue a) => AttributeKey a -> a -> Attribute
(:=) (AttributeKey key) value = Attribute
  { key: key
  , value: toValue value
  }

a :: [Attribute] -> [Content] -> Element
a attribs content = element "a" attribs (Just content)

div :: [Attribute] -> [Content] -> Element
div attribs content = element "div" attribs (Just content)

p :: [Attribute] -> [Content] -> Element
p attribs content = element "p" attribs (Just content)

img :: [Attribute] -> Element
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
  
  renderContent :: Maybe [Content] -> String
  renderContent Nothing = " />"
  renderContent (Just content) = 
    ">" ++ joinWith "" (map renderContentItem content) ++
    "</" ++ e.name ++ ">"
    where
    renderContentItem :: Content -> String
    renderContentItem (TextContent s) = s
    renderContentItem (ElementContent e) = render e

