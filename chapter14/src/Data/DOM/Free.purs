module Data.DOM.Free
  ( Element()
  , Attribute()
  , Content()
  , ContentF()
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
import Data.Foldable (for_)

import Control.Monad.Free
import Control.Monad.Writer
import Control.Monad.Writer.Class

newtype Element = Element
  { name         :: String
  , attribs      :: Array Attribute
  , content      :: Maybe (Content Unit)
  }

data ContentF a
  = TextContent String a
  | ElementContent Element a

instance functorContentF :: Functor ContentF where
  map f (TextContent s a) = TextContent s (f a)
  map f (ElementContent e a) = ElementContent e (f a)

type Content = Free ContentF

newtype Attribute = Attribute
  { key          :: String 
  , value        :: String
  }

newtype AttributeKey a = AttributeKey String

element :: String -> Array Attribute -> Maybe (Content Unit) -> Element
element name attribs content = Element
  { name:      name
  , attribs:   attribs
  , content:   content
  }

text :: String -> Content Unit
text s = liftF $ TextContent s unit

elem :: Element -> Content Unit
elem e = liftF $ ElementContent e unit

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

a :: Array Attribute -> Content Unit -> Element
a attribs content = element "a" attribs (Just content)

p :: Array Attribute -> Content Unit -> Element
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
render e = execWriter $ renderElement e
  where
  renderElement :: Element -> Writer String Unit
  renderElement (Element e) = do
    tell "<"
    tell e.name
    for_ e.attribs $ \a -> do
      tell " "
      renderAttribute a
    renderContent e.content
    
    where
    renderAttribute :: Attribute -> Writer String Unit
    renderAttribute (Attribute a) = do
      tell a.key
      tell "=\""
      tell a.value
      tell "\""
    
    renderContent :: Maybe (Content Unit) -> Writer String Unit
    renderContent Nothing = tell " />"
    renderContent (Just content) = do
      tell ">"
      runFreeM renderContentItem content
      tell "</"
      tell e.name
      tell ">"

    renderContentItem :: forall a. ContentF (Content a) -> Writer String (Content a)
    renderContentItem (TextContent s rest) = do
      tell s
      return rest
    renderContentItem (ElementContent e rest) = do
      renderElement e
      return rest

