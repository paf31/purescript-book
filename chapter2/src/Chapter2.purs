module Chapter2 where

import Math
import Debug.Trace

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

main = print (diagonal 3 4)
