{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module DigitalSearching
where
import Prelude hiding (lookup)

data family Map key :: * -> *

class (Ord key) => Key key where
  empty   ::  Map key val
  insert  ::  key -> (Maybe val -> val) -> Map key val -> Map key val
  lookup  ::  key -> Map key val -> Maybe val

data instance Map ()                 val  =  Empty | Single val

instance Key () where
  empty  =  Empty
  insert () f (Empty)     =  Single (f Nothing)
  insert () f (Single v)  =  Single (f (Just v))
  lookup () (Empty)     =  Nothing
  lookup () (Single v)  =  Just v

data instance Map (Either key1 key2) val  =

instance (Key key1, Key key2) => Key (Either key1 key2) where

data instance Map (key1, key2) val  =  

instance (Key key1, Key key2) => Key (key1, key2) where

type List elem  =  Either () (elem, [elem])

--toList :: [elem] -> List elem
--toList []        =  Left ()
--toList (a : as)  =  Right (a, as)

data instance Map [key] val  =

instance (Key key) => Key [key] where
