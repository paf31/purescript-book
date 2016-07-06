module Data.GameItem where

import Prelude

import Data.Maybe (Maybe(..))

data GameItem = Candle | Matches

instance showGameItem :: Show GameItem where
  show Candle         = "Candle"
  show Matches        = "Matches"

derive instance eqGameItem :: Eq GameItem
derive instance ordGameItem :: Ord GameItem

readItem :: String -> Maybe GameItem
readItem "Candle" = Just Candle
readItem "Matches" = Just Matches
readItem _ = Nothing
