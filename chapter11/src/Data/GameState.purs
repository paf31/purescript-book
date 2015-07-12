module Data.GameState where

import Prelude

import Data.Tuple
import Data.Coords
import Data.GameItem

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

newtype GameState = GameState
  { items       :: M.Map Coords (S.Set GameItem)
  , player      :: Coords
  , inventory   :: S.Set GameItem
  }

instance showGameState :: Show GameState where
  show (GameState o) =
    "GameState " ++
    "{ items: "     ++ show o.items ++
    ", player: "    ++ show o.player ++
    ", inventory: " ++ show o.inventory ++
    " }"

initialGameState :: GameState
initialGameState = GameState
  { items      : M.fromList $ L.toList [ Tuple (coords 0 1) (S.singleton Candle)
                                       , Tuple (coords 0 0) (S.singleton Matches)
                                       ]
  , player     : Coords { x: 0, y: 0 }
  , inventory  : S.empty
  }
