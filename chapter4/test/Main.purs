module Test.Main where

import Control.Monad.Eff.Console

import Data.Path
import Data.Foldable (for_)
import FileOperations

main = for_ (allFiles root) print
