module Control.Monad.Cont.Extras where

import Data.Array ()
import Data.Maybe
import Data.Tuple

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Ref

import Control.Monad.Trans
import Control.Monad.Cont.Trans

type WithRef eff = Eff (ref :: Ref | eff)

type ContRef eff = ContT Unit (WithRef eff)

foldC :: forall eff a r. (r -> a -> Tuple Boolean r) -> r -> ContRef eff a -> ContRef eff r
foldC f r0 c = do
  current <- lift $ newRef r0
  callCC $ \k -> do
    a <- c
    r <- lift $ readRef current
    case f r a of
      Tuple emit next -> do
        when emit $ k next 
        quietly $ lift $ writeRef current next

  where
  quietly :: forall m a. (Monad m) => ContT Unit m Unit -> ContT Unit m a
  quietly = withContT (\_ _ -> return unit)

collect :: forall eff a. ContRef eff (Maybe a) -> ContRef eff [a]
collect = foldC f []
  where
  f xs Nothing = Tuple true xs
  f xs (Just x) = Tuple false (xs ++ [x])
