module FileOperations where

import Data.Path
import Data.Array

allFiles :: Path -> [Path]
allFiles root = root : concatMap allFiles (ls root)
