module Split where

import Data.String (take, drop)
import Data.Either
import Data.Tuple

import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Control.Monad.Writer.Trans
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.Error.Trans
import Control.Monad.Identity

type Parser = StateT String (WriterT [String] (ErrorT String Identity))

split :: Parser String
split = do
  s <- get
  tell ["The state is " ++ show s]
  case s of
    "" -> throwError "Empty string"
    _ -> do
     put (drop 1 s)
     return (take 1 s)

runParser :: forall a. Parser a -> String -> Either String (Tuple (Tuple a String) [String])
runParser p s = runIdentity $ runErrorT $ runWriterT $ runStateT p s
