module Types where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Cont.Trans (ContT)

type Async eff = ContT Unit (Eff eff)
