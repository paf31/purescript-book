module Main where

import Data.Array (concatMap)

import Control.Monad (foldM)
import Control.Monad.Eff

import Graphics.Canvas hiding (translate)

lsystem :: forall a m s. (Monad m) =>
                         [a] ->
                         (a -> [a]) ->
                         (s -> a -> m s) ->
                         Number ->
                         s -> m s
lsystem init prod interpret n state = go init n
  where
  go s 0 = foldM interpret state s
  go s n = go (concatMap prod s) (n - 1)

data Alphabet = L | R | F

type Sentence = [Alphabet]

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

main = do
  canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  let
    initial :: Sentence
    initial = [F, R, R, F, R, R, F, R, R]

    productions :: Alphabet -> Sentence
    productions L = [L]
    productions R = [R]
    productions F = [F, L, F, R, R, F, L, F]

    interpret :: State -> Alphabet -> Eff (canvas :: Canvas) State
    interpret state L = return $ state { theta = state.theta - Math.pi / 3 }
    interpret state R = return $ state { theta = state.theta + Math.pi / 3 }
    interpret state F = do
      let x' = state.x + Math.cos state.theta * 1.5
          y' = state.y + Math.sin state.theta * 1.5
      moveTo ctx state.x state.y
      lineTo ctx x' y'
      return { x: x', y: y', theta: state.theta }

    initialState :: State
    initialState = { x: 120, y: 200, theta: 0 }

  setStrokeStyle "#000000" ctx

  strokePath ctx $ lsystem initial productions interpret 5 initialState
