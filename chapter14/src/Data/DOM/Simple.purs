module Data.DOM.Simple where

import Prelude

import Data.Maybe (Maybe(..))
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

render :: Element -> String
render (Element e) =
  "<" <> e.name <>
  " " <> joinWith " " (map renderAttribute e.attribs) <>
  renderContent e.content

  where
  renderAttribute :: Attribute -> String
  renderAttribute (Attribute x) = x.key <> "=\"" <> x.value <> "\""

  renderContent :: Maybe (Array Content) -> String
  renderContent Nothing = " />"
  renderContent (Just content) =
    ">" <> joinWith "" (map renderContentItem content) <>
    "</" <> e.name <> ">"
    where
    renderContentItem :: Content -> String
    renderContentItem (TextContent s) = s
    renderContentItem (ElementContent e) = render e
