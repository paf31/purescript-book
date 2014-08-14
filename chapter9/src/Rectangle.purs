module Main where

import Control.Monad.Eff

import Graphics.Canvas hiding (translate)

main = do
  canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle "#0000FF" ctx

  fillPath ctx $ rect ctx 
    { x: 250
    , y: 250
    , w: 100
    , h: 100
    }
