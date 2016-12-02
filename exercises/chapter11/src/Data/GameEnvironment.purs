module Data.GameEnvironment where

type PlayerName = String

newtype GameEnvironment = GameEnvironment
  { playerName    :: PlayerName
  , debugMode     :: Boolean
  }

gameEnvironment :: PlayerName -> Boolean -> GameEnvironment
gameEnvironment playerName debugMode = GameEnvironment
  { playerName    : playerName
  , debugMode     : debugMode
  }
