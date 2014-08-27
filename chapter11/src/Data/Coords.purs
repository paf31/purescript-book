module Data.Coords where

newtype Coords = Coords
  { x :: Number
  , y :: Number
  }

instance showCoords :: Show Coords where
  show (Coords p) = "Coords " ++
                    "{ x: " ++ show p.x ++
                    ", y: " ++ show p.y ++
                    " }"

instance eqCoords :: Eq Coords where
  (==) (Coords p1) (Coords p2) = p1.x == p2.x && p1.y == p2.y
  (/=) (Coords p1) (Coords p2) = p1.x /= p2.x || p1.y /= p2.y

instance ordCoords :: Ord Coords where
  compare (Coords p1) (Coords p2) =
    case compare p1.x p2.x of
      EQ -> compare p1.y p2.y
      other -> other

coords :: Number -> Number -> Coords
coords x y = Coords { x: x, y: y }

prettyPrintCoords :: Coords -> String
prettyPrintCoords (Coords p) = "(" ++ show p.x ++ ", " ++ show p.y ++ ")"
