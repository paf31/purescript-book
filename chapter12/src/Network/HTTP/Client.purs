module Network.HTTP.Client where

import Data.Maybe
import Data.Function

import Control.Monad.Eff
import Control.Monad.Eff.Ref

import Control.Monad.Trans
import Control.Monad.Cont.Trans
import Control.Monad.Cont.Extras

foreign import data HTTP :: !

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

type WithHTTP eff = Eff (http :: HTTP | eff)

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
  \}" :: forall eff. Fn3 Request 
                         (Chunk -> WithHTTP eff Unit) 
                         (WithHTTP eff Unit) 
                         (WithHTTP eff Unit)

getChunk :: forall eff. Request -> 
                        (Maybe Chunk -> WithHTTP eff Unit) -> 
                        WithHTTP eff Unit
getChunk req k = runFn3 getImpl req (k <<< Just) (k Nothing)

getCont :: forall eff. Request -> ContT Unit (WithHTTP eff) (Maybe Chunk)
getCont req = ContT $ getChunk req
 
getAll :: forall eff. Request -> ContT Unit (WithHTTP (ref :: Ref | eff)) Response
getAll req = Response <$> collect (getCont req)

