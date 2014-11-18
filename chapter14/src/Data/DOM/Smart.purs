module Data.DOM.Smart
  ( Element()
  , Attribute()
  , Content()
  , AttributeKey()

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

newtype AttributeKey = AttributeKey String

(:=) :: AttributeKey -> String -> Attribute
(:=) (AttributeKey key) value = Attribute
  { key: key
  , value: value
  }

a :: [Attribute] -> [Content] -> Element
a attribs content = element "a" attribs (Just content)

div :: [Attribute] -> [Content] -> Element
div attribs content = element "div" attribs (Just content)

p :: [Attribute] -> [Content] -> Element
p attribs content = element "p" attribs (Just content)

img :: [Attribute] -> Element
img attribs = element "img" attribs Nothing

href :: AttributeKey
href = AttributeKey "href"

_class :: AttributeKey
_class = AttributeKey "class"

src :: AttributeKey
src = AttributeKey "src"

width :: AttributeKey
width = AttributeKey "width"

height :: AttributeKey 
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

