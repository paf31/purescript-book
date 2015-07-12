module Example.Random where

import Prelude

import Data.Maybe
import Data.Array ((..))
import Data.Foldable (for_)

import Control.Monad.Eff
import Control.Monad.Eff.Random

import Graphics.Canvas

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle "#FF0000" ctx
  setStrokeStyle "#000000" ctx 

  for_ (1 .. 100) \_ -> do
    x <- random
    y <- random
    r <- random

    let path = arc ctx 
         { x     : x * 600.0
         , y     : y * 600.0
         , r     : r * 50.0
         , start : 0.0
         , end   : Math.pi * 2.0
         }
    
    fillPath ctx path
    strokePath ctx path

    return unit
