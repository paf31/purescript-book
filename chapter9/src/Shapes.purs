module Main where

import Control.Monad.Eff

import Graphics.Canvas hiding (translate)

translate :: forall r. Number -> Number -> 
              { x :: Number, y :: Number | r } -> 
              { x :: Number, y :: Number | r }
translate dx dy shape = shape 
  { x = shape.x + dx
  , y = shape.y + dy
  }

main = do
  canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle "#0000FF" ctx

  fillPath ctx $ rect ctx $ translate (-200) (-200)
    { x: 250
    , y: 250
    , w: 100
    , h: 100
    }
  
  setFillStyle "#00FF00" ctx

  fillPath ctx $ arc ctx $ translate 200 200 
    { x: 300
    , y: 300
    , r: 50 
    , start: Math.pi * 5 / 8
    , end: Math.pi * 2
    }
    
  setFillStyle "#FF0000" ctx

  fillPath ctx $ do
    moveTo ctx 300 260
    lineTo ctx 260 340
    lineTo ctx 340 340
    closePath ctx
