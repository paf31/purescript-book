module Main where

import Data.Maybe
import Data.Tuple
import Data.String
import Data.Foldable (for_)

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.RWS
import Control.Monad.RWS.Class

import Debug.Trace

import qualified Node.ReadLine as RL

import qualified Node.Yargs as Y
import qualified Node.Yargs.Setup as Y
import qualified Node.Yargs.Applicative as Y

import qualified Data.Map as M
import qualified Data.Set as S

type Lit = Boolean

data GameItem
  = Candle Lit
  | Matches

instance showGameItem :: Show GameItem where
  show (Candle true)  = "Lit Candle"
  show (Candle false) = "Candle"
  show Matches        = "Matches"

instance eqGameItem :: Eq GameItem where
  (==) (Candle l1) (Candle l2) = l1 == l2
  (==) Matches     Matches     = true
  (==) _           _           = false
  (/=) x           y           = not (x == y)

instance ordGameItem :: Ord GameItem where
  compare (Candle l1) (Candle l2) = compare l1 l2
  compare (Candle _)  Matches     = LT
  compare Matches     (Candle _)  = GT
  compare Matches     Matches     = EQ

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

newtype GameState = GameState
  { items       :: M.Map Coords (S.Set GameItem)
  , player      :: Coords
  , inventory   :: S.Set GameItem
  }

instance showGameState :: Show GameState where
  show (GameState o) = "GameState " ++
    "{ items: "     ++ show o.items ++
    ", player: "    ++ show o.player ++
    ", inventory: " ++ show o.inventory ++
    " }"

initialGameState :: GameState
initialGameState = GameState
  { items      : M.fromList [ Tuple (coords 0 0) (S.singleton (Candle false))
                            , Tuple (coords 1 0) (S.singleton Matches)
                            ]
  , player     : Coords { x: 0, y: 0 }
  , inventory  : S.empty
  }

type PlayerName = String

newtype GameEnvironment = GameEnvironment
  { playerName    :: PlayerName 
  }

gameEnvironment :: PlayerName -> GameEnvironment
gameEnvironment playerName = GameEnvironment 
  { playerName    : playerName
  }

type Log = [String] 

type Game = RWS GameEnvironment Log GameState

move :: Number -> Number -> Game Unit
move dx dy = modify (\(GameState state) -> GameState (state { player = updateCoords state.player }))
  where
  updateCoords :: Coords -> Coords
  updateCoords (Coords p) = coords (p.x + dx) (p.y + dy)

game :: [String] -> Game Unit
game ["look"] = do
  GameState state <- get
  tell ["You are at " ++ prettyPrintCoords state.player]
  for_ (M.lookup state.player state.items) $ \items ->
    tell ((\item -> "You can see the " ++ show item) <$> S.toList items)
game ["inventory"] = do
  GameState state <- get
  tell ((\item -> "You have the " ++ show item) <$> S.toList state.inventory)
game ["north"] = move 0    (-1)
game ["south"] = move 0    1
game ["west"]  = move (-1) 0
game ["east"]  = move 1    0
game ["debug"] = do
  state <- get
  tell [show state]
game [] = return unit
game _  = tell ["I don't understand"]

runGame :: GameEnvironment -> Eff (console :: RL.Console, trace :: Trace, ref :: Ref) Unit
runGame env = do
  interface <- RL.createInterface RL.process.stdin RL.process.stdout (\s -> return (Tuple [] s))
  RL.setPrompt "> " 2 interface

  ref <- newRef initialGameState

  let
    lineHandler :: String -> Eff (console :: RL.Console, trace :: Trace, ref :: Ref) Unit
    lineHandler input = do
      currentState <- readRef ref
      let result = runRWS (game (split " " input)) env currentState
      foreachE result.log trace
      writeRef ref result.state
      RL.prompt interface
      return unit

  RL.setLineHandler lineHandler interface
  RL.prompt interface

  return unit

main = Y.runY (Y.usage "$0 -p <player name>") $ 
  runGame <$> (gameEnvironment <$> Y.yarg "p" ["player"] 
                                          (Just "Player name") 
                                          (Just "The player name is required") 
                                          false)

