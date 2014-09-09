module Counter
  ( CounterF()
  , Counter()
  , Result()

  , reset
  , next
  , runCounter

  , test
  ) where

import Control.Monad.Free
import Control.Monad.Eff
import Control.Monad.ST

data CounterF a
  = Reset a
  | Next (Number -> a)

instance functorCounterF :: Functor CounterF where
  (<$>) f (Reset a) = Reset (f a)
  (<$>) f (Next k) = Next (f <<< k)

type Counter = Free CounterF

reset :: Counter Unit
reset = liftF $ Reset unit

next :: Counter Number
next = liftF $ Next id

type Result a = { result :: a, count :: Number }

runCounter :: forall a. Counter a -> Result a 
runCounter c = runPure (runST (do
  counter <- newSTRef 0
  let
    step (Reset more) = do
      writeSTRef counter 0
      return more
    step (Next more) = do
      n <- readSTRef counter
      writeSTRef counter (n + 1)
      return $ more n

  result <- goEff step c
  count <- readSTRef counter
  return { result: result, count: count}))

test :: Counter [Number]
test = do
  x <- next
  y <- next
  reset
  z <- next
  return [x, y, z]
