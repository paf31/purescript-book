module Types where

import Prelude

import Control.Monad.Eff
import Control.Monad.Cont.Trans

type Async eff = ContT Unit (Eff eff)
