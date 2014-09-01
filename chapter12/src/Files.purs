module Files where

import Data.Either
import Data.Function

import Control.Monad.Eff

import Control.Monad.Cont.Trans
import Control.Monad.Error.Trans

foreign import data FS :: !

type ErrorCode = String

type FilePath = String

foreign import readFileImpl
  "function readFileImpl(path, onSuccess, onFailure) {\
  \  return function() {\
  \    require('fs').readFile(path, { encoding: 'utf-8' }, function(error, data) {\
  \      if (error) {\
  \        onFailure(error.code)();\
  \      } else {\
  \        onSuccess(data)();\
  \      }\
  \    });\
  \  };\
  \}" :: forall eff. Fn3 FilePath 
                         (String -> Eff (fs :: FS | eff) Unit) 
                         (ErrorCode -> Eff (fs :: FS | eff) Unit) 
                         (Eff (fs :: FS | eff) Unit) 
                        
foreign import writeFileImpl
  "function writeFileImpl(path, data, onSuccess, onFailure) {\
  \  return function() {\
  \    require('fs').writeFile(path, data, { encoding: 'utf-8' }, function(error) {\
  \      if (error) {\
  \        onFailure(error.code)();\
  \      } else {\
  \        onSuccess();\
  \      }\
  \    });\
  \  };\
  \}" :: forall eff. Fn4 FilePath
                         String
                         (Eff (fs :: FS | eff) Unit) 
                         (ErrorCode -> Eff (fs :: FS | eff) Unit) 
                         (Eff (fs :: FS | eff) Unit) 
                       
readFile :: forall eff. FilePath -> (Either ErrorCode String -> Eff (fs :: FS | eff) Unit) -> Eff (fs :: FS | eff) Unit
readFile path k = runFn3 readFileImpl path (k <<< Right) (k <<< Left)

writeFile :: forall eff. FilePath -> String -> (Either ErrorCode Unit -> Eff (fs :: FS | eff) Unit) -> Eff (fs :: FS | eff) Unit
writeFile path text k = runFn4 writeFileImpl path text (k $ Right unit) (k <<< Left)

readFileCont :: forall eff. FilePath -> ErrorT ErrorCode (ContT Unit (Eff (fs :: FS | eff))) String
readFileCont path = ErrorT $ ContT $ readFile path

writeFileCont :: forall eff. FilePath -> String -> ErrorT ErrorCode (ContT Unit (Eff (fs :: FS | eff))) Unit
writeFileCont path text = ErrorT $ ContT $ writeFile path text

copyFileCont :: forall eff. FilePath -> FilePath -> ErrorT ErrorCode (ContT Unit (Eff (fs :: FS | eff))) Unit 
copyFileCont src dest = do
  content <- readFileCont src
  writeFileCont dest content
