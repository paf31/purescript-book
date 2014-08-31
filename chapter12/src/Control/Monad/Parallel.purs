module Control.Monad.Parallel where

import Data.Maybe

import Control.Monad.Eff
import Control.Monad.Eff.Ref

import Control.Monad.Cont.Trans

par :: forall a b r eff. (a -> b -> r) ->
                         ContT Unit (Eff (ref :: Ref | eff)) a ->
                         ContT Unit (Eff (ref :: Ref | eff)) b ->
                         ContT Unit (Eff (ref :: Ref | eff)) r
par f ca cb = ContT $ \k -> do
  ra <- newRef Nothing
  rb <- newRef Nothing
  runContT ca $ \a -> do
    mb <- readRef rb
    case mb of
      Nothing -> writeRef ra $ Just a
      Just b -> k (f a b)
  runContT cb $ \b -> do
    ma <- readRef ra
    case ma of
      Nothing -> writeRef rb $ Just b
      Just a -> k (f a b)

newtype Parallel eff a = Parallel (ContT Unit (Eff (ref :: Ref | eff)) a)

runParallel :: forall eff a. Parallel eff a -> ContT Unit (Eff (ref :: Ref | eff)) a
runParallel (Parallel c) = c

instance functorParallel :: Functor (Parallel eff) where
  (<$>) f (Parallel c) = Parallel (f <$> c)

instance applyParallel :: Apply (Parallel eff) where
  (<*>) (Parallel f) (Parallel x) = Parallel (par ($) f x)

instance applicativeParallel :: Applicative (Parallel eff) where
  pure a = Parallel $ pure a


