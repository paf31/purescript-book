module Main where

import Prelude

import Data.Maybe
import Data.String
import Data.Either
import Data.Foldable (for_)

import Data.Coords
import Data.GameItem
import Data.GameState
import Data.GameEnvironment

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.RWS
import Control.Monad.RWS.Class

import Game

import qualified Node.ReadLine as RL

import qualified Node.Yargs as Y
import qualified Node.Yargs.Setup as Y
import qualified Node.Yargs.Applicative as Y

runGame :: forall eff. GameEnvironment -> Eff (console :: CONSOLE | eff) Unit
runGame env = do
  interface <- RL.createInterface RL.noCompletion
  RL.setPrompt "> " 2 interface

  let
    lineHandler :: forall eff. GameState -> String -> Eff (console :: CONSOLE | eff) Unit
    lineHandler currentState input = do
      let result = runRWS (game (split " " input)) env currentState
      for_ result.log log
      RL.setLineHandler interface $ lineHandler result.state
      RL.prompt interface
      return unit

  RL.setLineHandler interface $ lineHandler initialGameState
  RL.prompt interface

  return unit

main = Y.runY (Y.usage "$0 -p <player name>") $ map runGame env
  where
  env :: Y.Y GameEnvironment
  env = gameEnvironment <$> Y.yarg "p" ["player"] 
                                   (Just "Player name") 
                                   (Right "The player name is required") 
                                   false
                        <*> Y.flag "d" ["debug"]
                                   (Just "Use debug mode")

