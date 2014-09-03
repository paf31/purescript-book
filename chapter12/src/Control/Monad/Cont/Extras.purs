module Control.Monad.Cont.Extras where

import Data.Array ()
import Data.Maybe
import Data.Either

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Ref

import Control.Monad.Trans
import Control.Monad.Cont.Trans

type WithRef eff = Eff (ref :: Ref | eff)

type ContRef eff = ContT Unit (WithRef eff)

foldC :: forall eff a b r. (b -> a -> Either b r) -> b -> ContRef eff a -> ContRef eff r
foldC f b0 c = do
  current <- lift $ newRef b0
  callCC $ \k -> quietly $ do
    a <- c
    b <- lift $ readRef current
    case f b a of
      Left next -> lift $ writeRef current next
      Right r -> k r

  where
  quietly :: forall m a b. (Monad m) => ContT Unit m b -> ContT Unit m a
  quietly = withContT (\_ _ -> return unit)

collect :: forall eff a. ContRef eff (Maybe a) -> ContRef eff [a]
collect = foldC f []
  where
  f xs Nothing = Right xs
  f xs (Just x) = Left (xs ++ [x])
