module Data.HRec where
    
import Prelude

import Data.Function

foreign import data HRec :: * -> *

instance showHRec :: (Show a) => Show (HRec a) where
  show rec = runFn3 foldHRec f "empty" rec
    where
      f = mkFn3 \s k a -> "runFn3 insert " ++ show k ++ " " ++ show a ++ " $ " ++ s

foreign import empty :: forall a. HRec a

foreign import insert :: forall a. Fn3 String a (HRec a) (HRec a)

foreign import mapHRec :: forall a b. Fn2 (a -> b) (HRec a) (HRec b)
                                                                    
foreign import foldHRec :: forall a r. Fn3 (Fn3 r String a r) r (HRec a) r
                                          
instance functorHRec :: Functor HRec where
  map f rec = runFn2 mapHRec f rec
