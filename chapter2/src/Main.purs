module Main where

import Prelude ((+), (*))
import Math (sqrt)
import Control.Monad.Eff.Console (print)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

main = print (diagonal 3.0 4.0)
