module Data.GameEnvironment where

type PlayerName = String

newtype GameEnvironment = GameEnvironment
  { playerName    :: PlayerName
  }

gameEnvironment :: PlayerName -> GameEnvironment
gameEnvironment playerName = GameEnvironment
  { playerName    : playerName
  }
