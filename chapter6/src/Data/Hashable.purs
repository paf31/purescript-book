module Data.Hashable 
  ( HashCode()
  , hashCode

  , Hashable
  , hash
  , hashEqual
  ) where

import Prelude 

import Data.Maybe
import Data.Tuple
import Data.Either
import Data.String
import Data.Char
import Data.Function (on)
import Data.Foldable (foldMap)
import Data.Monoid (Monoid)

newtype HashCode = HashCode Int

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65536)

class (Eq a) <= Hashable a where
  hash :: a -> HashCode

instance showHashCode :: Show HashCode where
  show (HashCode h) = "(HashCode " ++ show h ++ ")"

instance eqHashCode :: Eq HashCode where
  eq (HashCode h1) (HashCode h2) = h1 == h2

instance semigroupHashCode :: Semigroup HashCode where
  append (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

instance monoidHashCode :: Monoid HashCode where
  mempty = hashCode 0

hashEqual :: forall a. (Hashable a) => a -> a -> Boolean
hashEqual = eq `on` hash

instance hashChar :: Hashable Char where
  hash = hash <<< toCharCode

instance hashString :: Hashable String where
  hash = hash <<< toCharArray

instance hashInt :: Hashable Int where
  hash = hashCode

instance hashBoolean :: Hashable Boolean where
  hash false = hashCode 0
  hash true  = hashCode 1

instance hashArray :: (Hashable a) => Hashable (Array a) where
  hash = foldMap hash

instance hashMaybe :: (Hashable a) => Hashable (Maybe a) where
  hash Nothing = hashCode 0
  hash (Just a) = hashCode 1 <> hash a

instance hashTuple :: (Hashable a, Hashable b) => Hashable (Tuple a b) where
  hash (Tuple a b) = hash a <> hash b

instance hashEither :: (Hashable a, Hashable b) => Hashable (Either a b) where
  hash (Left a) = hashCode 0 <> hash a
  hash (Right b) = hashCode 1 <> hash b
