module Test.Main where

import Control.Monad.Eff.Console

import Data.Picture

circle :: Shape
circle = Circle (Point { x: 0.0, y: 0.0 }) 10.0

rectangle :: Shape
rectangle = Rectangle (Point { x: 10.0, y: 10.0 }) 10.0 10.0

picture :: Picture
picture = [circle, rectangle]

main = log (showBounds (bounds picture))
