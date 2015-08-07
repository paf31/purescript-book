module Data.DOM.Name
  ( Element()
  , Attribute()
  , Name()
  , Content()
  , ContentF()
  , AttributeKey()
  , IsValue
  , toValue
  , Href(..)

  , a
  , p
  , img 

  , href
  , _class
  , src
  , width
  , height
  , name

  , (:=)
  , text
  , elem
  , newName 

  , render
  ) where

import Prelude

import Data.Maybe
import Data.Tuple (snd)
import Data.Foldable (for_)

import Control.Monad.Free
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Control.Monad.Writer.Trans
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans

newtype Element = Element
  { name         :: String
  , attribs      :: Array Attribute
  , content      :: Maybe (Content Unit)
  }

newtype Name = Name String

data ContentF a
  = TextContent String a
  | ElementContent Element a
  | NewName (Name -> a)

instance functorContentF :: Functor ContentF where
  map f (TextContent s a) = TextContent s (f a)
  map f (ElementContent e a) = ElementContent e (f a)
  map f (NewName k) = NewName (f <<< k)

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

newName :: Content Name
newName = liftF $ NewName id

class IsValue a where
  toValue :: a -> String

instance stringIsValue :: IsValue String where
  toValue = id
 
instance intIsValue :: IsValue Int where
  toValue = show
 
instance numberIsValue :: IsValue Number where
  toValue = show

instance nameIsValue :: IsValue Name where
  toValue (Name n) = n

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

data Href
  = URLHref String
  | AnchorHref Name

instance hrefIsValue :: IsValue Href where
  toValue (URLHref url) = url
  toValue (AnchorHref (Name nm)) = "#" ++ nm

href :: AttributeKey Href
href = AttributeKey "href"

name :: AttributeKey Name 
name = AttributeKey "name"

_class :: AttributeKey String
_class = AttributeKey "class"

src :: AttributeKey String
src = AttributeKey "src"

width :: AttributeKey Number
width = AttributeKey "width"

height :: AttributeKey Number
height = AttributeKey "height"

type Interp = WriterT String (State Int)

render :: Element -> String
render e = evalState (execWriterT (renderElement e)) 0
  where
  renderElement :: Element -> Interp Unit
  renderElement (Element e) = do
    tell "<"
    tell e.name
    for_ e.attribs $ \a -> do
      tell " "
      renderAttribute a
    renderContent e.content
    
    where
    renderAttribute :: Attribute -> Interp Unit
    renderAttribute (Attribute a) = do
      tell a.key
      tell "=\""
      tell a.value
      tell "\""
    
    renderContent :: Maybe (Content Unit) -> Interp Unit
    renderContent Nothing = tell " />"
    renderContent (Just content) = do
      tell ">"
      runFreeM renderContentItem content
      tell "</"
      tell e.name
      tell ">"

    renderContentItem :: forall a. ContentF (Content a) -> Interp (Content a)
    renderContentItem (TextContent s rest) = do
      tell s
      return rest
    renderContentItem (ElementContent e rest) = do
      renderElement e
      return rest
    renderContentItem (NewName k) = do
      n <- get
      let name = Name $ "name" ++ show n
      put $ n + 1
      return (k name)

