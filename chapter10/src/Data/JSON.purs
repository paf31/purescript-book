module Data.JSON where

import Data.Foreign (Foreign)

foreign import stringify :: Foreign -> String
