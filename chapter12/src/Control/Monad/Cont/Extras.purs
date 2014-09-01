module Control.Monad.Cont.Extras where

import Data.Array ()
import Data.Maybe

import Control.Monad.Eff
import Control.Monad.Eff.Ref

import Control.Monad.Trans
import Control.Monad.Cont.Trans

type WithRef eff = Eff (ref :: Ref | eff)

collect :: forall eff a. ContT Unit (WithRef eff) (Maybe a) -> 
                         ContT Unit (WithRef eff) [a]
collect c = do
  r <- lift $ newRef []
  callCC $ \k -> do
    m <- c
    xs <- lift $ readRef r
    case m of
      Nothing -> k xs
      Just x -> quietly $ lift $ writeRef r (xs ++ [x])

  where
  quietly :: forall m a. (Monad m) => ContT Unit m Unit -> ContT Unit m a
  quietly = withContT (\_ _ -> return unit)

