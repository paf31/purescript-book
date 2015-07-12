module Example.Rectangle where

import Prelude

import Data.Maybe

import Control.Monad.Eff

import Graphics.Canvas

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle "#0000FF" ctx

  fillPath ctx $ rect ctx 
    { x: 250.0
    , y: 250.0
    , w: 100.0
    , h: 100.0
    }
