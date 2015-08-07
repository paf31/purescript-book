module Split where

import Prelude

import Data.String (take, drop, toUpper, toLower)
import Data.Either
import Data.Tuple
import Data.Identity

import Control.MonadPlus
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Control.Monad.Writer.Trans
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Except.Trans

type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

split :: Parser String
split = do
  s <- get
  tell ["The state is " ++ show s]
  case s of
    "" -> throwError ["Empty string"]
    _ -> do
     put (drop 1 s)
     return (take 1 s)

eof :: Parser Unit
eof = do
  s <- get
  tell ["The state is " ++ show s]
  case s of
    "" -> return unit
    _ -> throwError ["Expected end-of-file"]

upper :: Parser String
upper = do
  s <- split
  guard $ toUpper s == s
  return s

lower :: Parser String
lower = do
  s <- split
  guard $ toLower s == s
  return s

runParser :: forall a. Parser a -> String -> Either Errors (Tuple (Tuple a String) Log)
runParser p = runExcept <<< runWriterT <<< runStateT p
