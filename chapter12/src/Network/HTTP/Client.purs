module Network.HTTP.Client where

import Data.Maybe
import Data.Function

import Control.Monad.Eff
import Control.Monad.Eff.Ref

import Control.Monad.Trans
import Control.Monad.Cont.Trans
import Control.Monad.Cont.Extras

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

type FutureHTTP f eff = Eff (future :: Future (http :: HTTP | f) | eff)

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
                               (Chunk -> FutureHTTP f eff a) 
                               (FutureHTTP f eff b) 
                               (FutureHTTP f eff Unit)

getChunk :: forall eff f a. Request -> 
                            (Maybe Chunk -> FutureHTTP f eff a) -> 
                            FutureHTTP f eff Unit
getChunk req k = runFn3 getImpl req (k <<< Just) (k Nothing)

getCont :: forall eff f. Request -> ContT Unit (FutureHTTP f eff) (Maybe Chunk)
getCont req = ContT $ getChunk req
 
getAll :: forall eff f. Request -> ContT Unit (FutureHTTP f (ref :: Ref | eff)) Response
getAll req = Response <$> collect (getCont req)

