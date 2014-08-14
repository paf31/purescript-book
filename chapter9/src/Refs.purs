module Main where

import Data.Traversable (for)

import Control.Monad.Eff
import Control.Monad.Eff.DOM
import Control.Monad.Eff.Ref

import Graphics.Canvas

import Debug.Trace

main = do
  canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  clickCount <- newRef 0

  node <- querySelector "#canvas"
  for node $ addEventListener "click" $ do
    trace "Mouse clicked!"

    modifyRef clickCount (\count -> count + 1)

    count <- readRef clickCount
    
    flip setFillStyle ctx $ 
      case count % 3 of
        0 -> "#FF0000" 
	1 -> "#00FF00"
        2 -> "#0000FF"

    fillPath ctx $ do
      moveTo ctx 300 300
      arc ctx
          { x     : 300
          , y     : 300
          , r     : 250
          , start : Math.pi * (count - 1) / 4
          , end   : Math.pi * count / 4
          }
      closePath ctx

    return unit
