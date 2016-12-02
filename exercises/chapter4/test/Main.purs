module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Path (root)
import Data.Foldable (for_)
import FileOperations (allFiles)

main :: Eff (console :: CONSOLE) Unit
main = for_ (allFiles root) logShow
