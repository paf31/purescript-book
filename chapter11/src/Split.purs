module Split where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT, throwError)
import Control.Monad.State.Trans (StateT, runStateT, get, put)
import Control.Monad.Writer.Trans (WriterT, runWriterT, tell)
import Control.MonadPlus (guard)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.String (take, drop, toUpper, toLower)
import Data.Tuple (Tuple)

type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

split :: Parser String
split = do
  s <- get
  tell ["The state is " <> show s]
  case s of
    "" -> throwError ["Empty string"]
    _ -> do
     put (drop 1 s)
     pure (take 1 s)

eof :: Parser Unit
eof = do
  s <- get
  tell ["The state is " <> show s]
  case s of
    "" -> pure unit
    _ -> throwError ["Expected end-of-file"]

upper :: Parser String
upper = do
  s <- split
  guard $ toUpper s == s
  pure s

lower :: Parser String
lower = do
  s <- split
  guard $ toLower s == s
  pure s

runParser :: forall a. Parser a -> String -> Either Errors (Tuple (Tuple a String) Log)
runParser p = runExcept <<< runWriterT <<< runStateT p
