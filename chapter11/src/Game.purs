module Game where

import Data.Maybe
import Data.Foldable (for_)

import Data.Coords
import Data.GameItem
import Data.GameState
import Data.GameEnvironment

import Control.Monad.RWS
import Control.Monad.RWS.Class

import qualified Data.Map as M
import qualified Data.Set as S

type Log = [String]

type Game = RWS GameEnvironment Log GameState

describeRoom :: Game Unit
describeRoom = do
  GameState state <- get
  case state.player of
    Coords { x = 0, y = 0 } -> tell ["You are in a dark forest. You see a path to the north."]
    Coords { x = 0, y = 1 } -> tell ["You are in a clearing."]
    _ -> tell ["You are deep in the forest."]

pickUp :: GameItem -> Game Unit
pickUp item = do
  GameState state <- get
  case state.player `M.lookup` state.items of
    Just items | item `S.member` items -> do
      let newItems = M.update (Just <<< S.delete item) state.player state.items
          newInventory = S.insert item state.inventory
      put $ GameState state { items     = newItems
                            , inventory = newInventory
                            }
      tell ["You now have the " ++ show item]
    _ -> tell ["I don't see that item here."]

move :: Number -> Number -> Game Unit
move dx dy = modify (\(GameState state) -> GameState (state { player = updateCoords state.player }))
  where
  updateCoords :: Coords -> Coords
  updateCoords (Coords p) = coords (p.x + dx) (p.y + dy)

has :: GameItem -> Game Boolean
has item = do
  GameState state <- get
  return $ item `S.member` state.inventory

use :: GameItem -> Game Unit
use Candle = tell ["I don't know what you want me to do with that."]
use Matches = do
  hasCandle <- has Candle
  if hasCandle
    then do
      GameEnvironment env <- ask
      tell [ "You light the candle."
           , "Congratulations, " ++ env.playerName ++ "!"
           , "You win!"
           ]
    else tell ["You don't have anything to light."]

game :: [String] -> Game Unit
game ["look"] = do
  GameState state <- get
  tell ["You are at " ++ prettyPrintCoords state.player]
  describeRoom
  for_ (M.lookup state.player state.items) $ \items ->
    tell ((\item -> "You can see the " ++ show item ++ ".") <$> S.toList items)
game ["inventory"] = do
  GameState state <- get
  tell ((\item -> "You have the " ++ show item ++ ".") <$> S.toList state.inventory)
game ["north"] = move 0    1
game ["south"] = move 0    (-1)
game ["west"]  = move (-1) 0
game ["east"]  = move 1    0
game ["take", item] =
  case readItem item of
    Nothing -> tell ["I don't know what item you are referring to."]
    Just gameItem -> pickUp gameItem
game ["use", item] =
  case readItem item of
    Nothing -> tell ["I don't know what item you are referring to."]
    Just gameItem -> do
      hasItem <- has gameItem
      if hasItem
        then use gameItem
        else tell ["You don't have that item."]
game ["debug"] = do
  GameEnvironment env <- ask
  if env.debugMode
    then do
      state <- get
      tell [show state]
    else tell ["Not running in debug mode."]  
game [] = return unit
game _  = tell ["I don't understand."]

