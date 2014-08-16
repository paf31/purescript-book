module Main where

import Data.Traversable (for)

import Control.Monad.Eff
import Control.Monad.Eff.DOM
import Control.Monad.Eff.Ref

import Graphics.Canvas

import Debug.Trace

render :: forall eff. Number -> Context2D -> Eff (canvas :: Canvas | eff) Context2D
render count ctx = do
  setFillStyle "#FFFFFF" ctx

  fillPath ctx $ rect ctx
    { x: 0
    , y: 0
    , w: 600
    , h: 600
    }
  
  setFillStyle "#00FF00" ctx 

  withContext ctx $ do
    let scaleX = Math.sin (count * Math.pi / 4) + 1.5
    let scaleY = Math.sin (count * Math.pi / 6) + 1.5

    translate { translateX:  300, translateY:  300 } ctx
    rotate (count * Math.pi / 18) ctx
    scale { scaleX: scaleX, scaleY: scaleY } ctx 
    translate { translateX: -100, translateY: -100 } ctx

    fillPath ctx $ rect ctx
      { x: 0
      , y: 0
      , w: 200
      , h: 200
      }

main = do
  canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  clickCount <- newRef 0

  render 0 ctx

  node <- querySelector "#canvas"
  for node $ addEventListener "click" $ do
    trace "Mouse clicked!"

    modifyRef clickCount (\count -> count + 1)

    count <- readRef clickCount

    render count ctx

    return unit
