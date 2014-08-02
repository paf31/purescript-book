module FileOperations where

import Data.Path
import Data.Array

allFiles :: Path -> [Path]
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> [Path]
allFiles' file = file : do
  child <- ls file
  allFiles' child
