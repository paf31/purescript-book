module Network.HTTP.Client where

import Data.Maybe
import Data.Function

import Control.Monad.Eff
import Control.Monad.Cont.Trans

foreign import data HTTP :: !

foreign import data Future :: # ! -> !

newtype Request = Request
  { host :: String
  , path :: String
  }

newtype Chunk = Chunk String

instance showChunk :: Show Chunk where
  show (Chunk s) = "Chunk " ++ show s

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

get :: forall eff f a. Request -> 
                       (Maybe Chunk -> Eff (future :: Future (http :: HTTP | f) | eff) a) -> 
                       Eff (future :: Future (http :: HTTP | f) | eff) Unit
get req k = runFn3 getImpl req (k <<< Just) (k Nothing)

getCont :: forall eff f. Request -> ContT Unit (Eff (future :: Future (http :: HTTP | f) | eff)) (Maybe Chunk)
getCont req = ContT $ get req


