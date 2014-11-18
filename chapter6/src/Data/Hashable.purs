module Data.Hashable where

import Prelude hiding ((<#>))

import Data.Maybe
import Data.Tuple
import Data.Either
import Data.String
import Data.Char

import Data.Function

type HashCode = Number

class (Eq a) <= Hashable a where
  hash :: a -> HashCode

(<#>) :: HashCode -> HashCode -> HashCode
(<#>) h1 h2 = (73 * h1 + 51 * h2) % 65536

hashEqual :: forall a. (Hashable a) => a -> a -> Boolean
hashEqual = (==) `on` hash

instance hashChar :: Hashable Char where
  hash = toCharCode

instance hashString :: Hashable String where
  hash = hash <<< toCharArray

instance hashNumber :: Hashable Number where
  hash n = hash (show n)

instance hashBoolean :: Hashable Boolean where
  hash false = 0
  hash true  = 1

instance hashArray :: (Hashable a) => Hashable [a] where
  hash [] = 0
  hash (x : xs) = hash x <#> hash xs

instance hashMaybe :: (Hashable a) => Hashable (Maybe a) where
  hash Nothing = 0
  hash (Just a) = 1 <#> hash a

instance hashTuple :: (Hashable a, Hashable b) => Hashable (Tuple a b) where
  hash (Tuple a b) = hash a <#> hash b

instance hashEither :: (Hashable a, Hashable b) => Hashable (Either a b) where
  hash (Left a) = 0 <#> hash a
  hash (Right b) = 1 <#> hash b
