{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module DigitalSearching
where
import Prelude hiding (lookup)
import Data.Maybe

-- Steven Wallis de Vries - s1011387
-- Ciske Harsema - s1010048

data family Map key :: * -> *

class (Ord key) => Key key where
  empty  :: Map key val
  insert :: key -> (Maybe val -> val) -> Map key val -> Map key val
  lookup :: key -> Map key val -> Maybe val

data instance Map () val = Empty | Single val

instance Key () where
  empty = Empty
  insert () valCons Empty      = Single (valCons Nothing)
  insert () valCons (Single v) = Single (valCons (Just v))
  lookup () Empty              = Nothing
  lookup () (Single v)         = Just v


-- 12.3.1
-- We split left and right because these keys can never be equal
data instance Map (Either key1 key2) val = EMap (Map key1 val) (Map key2 val)

instance (Key key1, Key key2) => Key (Either key1 key2) where
  empty = EMap empty empty
  insert (Left key)  valCons (EMap left right) = EMap (insert key valCons left) right
  insert (Right key) valCons (EMap left right) = EMap left (insert key valCons right)
  lookup (Left key)  (EMap left _)  = lookup key left
  lookup (Right key) (EMap _ right) = lookup key right


-- 12.3.2
-- The outer map used the first element as key, because the first elements are compared first
data instance Map (key1, key2) val = T2Map (Map key1 (Map key2 val))

instance (Key key1, Key key2) => Key (key1, key2) where
  empty = T2Map empty
  insert (k1, k2) valCons (T2Map m1) = T2Map $ insert k1 (insert k2 valCons . fromMaybe empty) m1
  lookup (k1, k2) (T2Map m1) = case lookup k1 m1 of {Nothing -> Nothing; Just m2 -> lookup k2 m2}

-- 12.3.3
type List elem = Either () (elem, [elem])

toList :: [elem] -> List elem
toList []       = Left ()
toList (a : as) = Right (a, as)

data instance Map [key] val = LMap (Map (List key) val)

instance (Key key) => Key [key] where
  empty = LMap empty
  insert keyl valCons (LMap lm) = LMap $ insert (toList keyl) valCons lm
  lookup keyl (LMap lm) = lookup (toList keyl) lm

-- Oops apparently there was a hint on this at the bottom of the exercise...


-- Here is an attempt to apply the same reaoning to binary trees:
-- In this case we will order them first on the left tree, then on the element and then on the right tree

data Tree elem = TEmpty | Node (Tree elem) elem (Tree elem)
  deriving(Eq, Ord)

type Tree2 elem = Either () (Tree elem, (elem, Tree elem))

toTree2 :: Tree elem -> Tree2 elem
toTree2 TEmpty       = Left ()
toTree2 (Node l e r) = Right (l, (e, r))

data instance Map (Tree key) val = TMap (Map (Tree2 key) val)

instance (Key key) => Key (Tree key) where
  empty = TMap empty
  insert keyt valCons (TMap tm) = TMap $ insert (toTree2 keyt) valCons tm
  lookup keyt (TMap tm) = lookup (toTree2 keyt) tm
