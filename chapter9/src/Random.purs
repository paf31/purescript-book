module Main where

import Control.Monad.Eff
import Control.Monad.Eff.Random

import Graphics.Canvas

main = do
  canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle "#FF0000" ctx
  setStrokeStyle "#000000" ctx 

  forE 1 100 $ \_ -> do
    x <- random
    y <- random
    r <- random

    let path = arc ctx 
         { x     : x * 600
         , y     : y * 600
         , r     : r * 50
         , start : 0
         , end   : Math.pi * 2 
         }
    
    fillPath ctx path
    strokePath ctx path

    return unit
