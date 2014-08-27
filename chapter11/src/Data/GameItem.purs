module Data.GameItem where

import Data.Maybe

data GameItem = Candle | Matches

instance showGameItem :: Show GameItem where
  show Candle         = "Candle"
  show Matches        = "Matches"

instance eqGameItem :: Eq GameItem where
  (==) Candle      Candle      = true
  (==) Matches     Matches     = true
  (==) _           _           = false
  (/=) x           y           = not (x == y)

instance ordGameItem :: Ord GameItem where
  compare Candle  Candle   = EQ
  compare Candle  Matches  = LT
  compare Matches Candle   = GT
  compare Matches Matches  = EQ

readItem :: String -> Maybe GameItem
readItem "Candle" = Just Candle
readItem "Matches" = Just Matches
readItem _ = Nothing
