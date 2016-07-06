module Files where

import Prelude

import Control.Monad.Cont.Trans (ContT(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn4, Fn3, runFn4, runFn3)
import Types (Async)

foreign import data FS :: !

type ErrorCode = String

type FilePath = String

foreign import readFileImpl ::
                 forall eff. Fn3 FilePath
                   (String -> Eff (fs :: FS | eff) Unit)
                   (ErrorCode -> Eff (fs :: FS | eff) Unit)
                   (Eff (fs :: FS | eff) Unit)

foreign import writeFileImpl ::
                 forall eff. Fn4 FilePath
                   String
                   (Eff (fs :: FS | eff) Unit)
                   (ErrorCode -> Eff (fs :: FS | eff) Unit)
                   (Eff (fs :: FS | eff) Unit)

readFile :: forall eff. FilePath -> (Either ErrorCode String -> Eff (fs :: FS | eff) Unit) -> Eff (fs :: FS | eff) Unit
readFile path k = runFn3 readFileImpl path (k <<< Right) (k <<< Left)

writeFile :: forall eff. FilePath -> String -> (Either ErrorCode Unit -> Eff (fs :: FS | eff) Unit) -> Eff (fs :: FS | eff) Unit
writeFile path text k = runFn4 writeFileImpl path text (k $ Right unit) (k <<< Left)

readFileCont :: forall eff. FilePath -> Async (fs :: FS | eff) (Either ErrorCode String)
readFileCont path = ContT $ readFile path

writeFileCont :: forall eff. FilePath -> String -> Async (fs :: FS | eff) (Either ErrorCode Unit)
writeFileCont path text = ContT $ writeFile path text

readFileContEx :: forall eff. FilePath -> ExceptT ErrorCode (Async (fs :: FS | eff)) String
readFileContEx path = ExceptT $ readFileCont path

writeFileContEx :: forall eff. FilePath -> String -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
writeFileContEx path text = ExceptT $ writeFileCont path text

copyFileContEx :: forall eff. FilePath -> FilePath -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
copyFileContEx src dest = do
  content <- readFileContEx src
  writeFileContEx dest content
