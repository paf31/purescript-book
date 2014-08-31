module Network.HTTP.Client where

import Data.Maybe
import Data.Function

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Ref.Unsafe

import Control.Monad.Trans
import Control.Monad.Cont.Trans
import Control.Monad.State.Trans
import Control.Monad.State.Class

foreign import data HTTP :: !

foreign import data Future :: # ! -> !

newtype Request = Request
  { host :: String
  , path :: String
  }

newtype Chunk = Chunk String

instance showChunk :: Show Chunk where
  show (Chunk s) = "Chunk " ++ show s

newtype Response = Response [Chunk]

instance showResponse :: Show Response where
  show (Response cs) = "Response " ++ show cs

runChunk :: Chunk -> String
runChunk (Chunk s) = s

foreign import getImpl
  "function getImpl(opts, more, done) {\
  \  return function() {\
  \    require('http').request(opts, function(res) {\
  \      res.setEncoding('utf8');\
  \      res.on('data', function (s) {\
  \        more(s)();\
  \      });\
  \      res.on('end', function () {\
  \        done();\
  \      });\
  \    }).end();\
  \  };\
  \}" :: forall eff f a b. Fn3 Request 
                               (Chunk -> Eff (future :: Future (http :: HTTP | f) | eff) a) 
                               (Eff (future :: Future (http :: HTTP | f) | eff) b) 
                               (Eff (future :: Future (http :: HTTP | f) | eff) Unit)

getChunk :: forall eff f a. Request -> 
                            (Maybe Chunk -> Eff (future :: Future (http :: HTTP | f) | eff) a) -> 
                            Eff (future :: Future (http :: HTTP | f) | eff) Unit
getChunk req k = runFn3 getImpl req (k <<< Just) (k Nothing)

getCont :: forall eff f. Request -> ContT Unit (Eff (future :: Future (http :: HTTP | f) | eff)) (Maybe Chunk)
getCont req = ContT $ getChunk req
 
quietly :: forall m a. (Monad m) => ContT Unit m Unit -> ContT Unit m a
quietly = withContT (\_ _ -> return unit)

collect :: forall eff a. ContT Unit (Eff (ref :: Ref | eff)) (Maybe a) -> ContT Unit (Eff (ref :: Ref | eff)) [a]
collect c = do
  r <- lift $ newRef []	
  callCC $ \k -> do
    m <- c
    xs <- lift $ readRef r 
    case m of
      Nothing -> k xs
      Just x -> do
        quietly $ lift $ writeRef r (xs ++ [x])

getAll :: forall eff f. Request -> ContT Unit (Eff (ref :: Ref, future :: Future (http :: HTTP | f) | eff)) Response
getAll req = Response <$> collect (getCont req)
