module Main where

import Control.Monad.Eff

import Graphics.Canvas hiding (translate)

import Control.Monad.Reader
import Control.Monad.Reader.Trans

type AppMonad = ReaderT Context2D (Eff (canvas :: Canvas))

setFillStyleR :: String -> AppMonad Unit
setFillStyleR style = ReaderT $ \ctx -> do
  setFillStyle style ctx
  return unit

fillPathR :: AppMonad Unit -> AppMonad Unit
fillPathR path = ReaderT $ \ctx -> do
  fillPath ctx $ runReaderT path ctx
  return unit

rectR :: Rectangle -> AppMonad Unit
rectR r = ReaderT $ \ctx -> do
  rect ctx r
  return unit

main = do
  canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  flip runReaderT ctx $ do
    setFillStyleR "#0000FF"

    fillPathR $ rectR
      { x: 250
      , y: 250
      , w: 100
      , h: 100
      }
