module Data.Coords where

import Prelude

newtype Coords = Coords
  { x :: Int
  , y :: Int
  }

instance showCoords :: Show Coords where
  show (Coords p) = "Coords " <>
                    "{ x: " <> show p.x <>
                    ", y: " <> show p.y <>
                    " }"

derive instance eqCoords :: Eq Coords
derive instance ordCoords :: Ord Coords

coords :: Int -> Int -> Coords
coords x y = Coords { x: x, y: y }

prettyPrintCoords :: Coords -> String
prettyPrintCoords (Coords p) = "(" <> show p.x <> ", " <> show p.y <> ")"
