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

    setFillStyle "#FFFFFF" ctx

    fillPath ctx $ rect ctx
      { x: 0
      , y: 0
      , w: 600
      , h: 600
      }

    setFillStyle "#00FF00" ctx 

    withContext ctx $ do
      translate { translateX:  300, translateY:  300 } ctx
      rotate (count * Math.pi / 18) ctx
      translate { translateX: -100, translateY: -100 } ctx

      fillPath ctx $ rect ctx
        { x: 0
        , y: 0
        , w: 200
        , h: 200
        }

    return unit
