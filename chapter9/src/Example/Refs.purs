module Example.Refs where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.DOM (addEventListener, querySelector)
import Control.Monad.Eff.Ref (REF, readRef, modifyRef, newRef)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import Graphics.Canvas (Context2D, CANVAS, getContext2D, getCanvasElementById,
                        rect, fillPath, translate, scale, rotate, withContext,
                        setFillStyle)
import Math as Math
import Partial.Unsafe (unsafePartial)

render :: forall eff. Int -> Context2D -> Eff (canvas :: CANVAS | eff) Context2D
render count ctx = do
  setFillStyle "#FFFFFF" ctx

  fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , w: 600.0
    , h: 600.0
    }

  setFillStyle "#00FF00" ctx

  withContext ctx do
    let scaleX = Math.sin (toNumber count * Math.pi / 4.0) + 1.5
    let scaleY = Math.sin (toNumber count * Math.pi / 6.0) + 1.5

    translate { translateX: 300.0, translateY:  300.0 } ctx
    rotate (toNumber count * Math.pi / 18.0) ctx
    scale { scaleX: scaleX, scaleY: scaleY } ctx
    translate { translateX: -100.0, translateY: -100.0 } ctx

    fillPath ctx $ rect ctx
      { x: 0.0
      , y: 0.0
      , w: 200.0
      , h: 200.0
      }

main :: Eff ( canvas :: CANVAS
            , ref :: REF
            , dom :: DOM
            , console :: CONSOLE
            ) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  clickCount <- newRef 0

  render 0 ctx

  node <- querySelector "#canvas"
  for_ node $ addEventListener "click" $ void do
    log "Mouse clicked!"
    modifyRef clickCount \count -> count + 1
    count <- readRef clickCount
    render count ctx
