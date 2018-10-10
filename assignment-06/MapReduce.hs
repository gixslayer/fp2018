module MapReduce
where
import Prelude --hiding (Monoid)
import Hardware

--class Monoid a where
--  mempty  ::  a
--  mapply  ::  a -> a -> a

reduce  ::  (Monoid m) => [m] -> m
reduce  =  foldr mappend mempty

-- 6.2.1
-- Not sure if these are all Monoids, but I think so
newtype And = B1 {fromB1::Bool}
  deriving (Show)

instance Monoid And where
  mempty = B1 True
  mappend x y = B1 (fromB1 x && fromB1 y)

newtype Or = B2 {fromB2::Bool}
  deriving (Show)

instance Monoid Or where
  mempty = B2 False
  mappend x y = B2 (fromB2 x || fromB2 y)

-- 6.2.2
my_and :: [Bool] -> Bool
my_and = fromB1 . reduce . map B1

my_or :: [Bool] -> Bool
my_or = fromB2 . reduce . map B2

-- 6.3
-- Not sure if it works correctly, but I think so
newtype OrdList elem = Ord [elem]
  deriving (Show)

insert :: (Ord elem) => elem -> OrdList elem -> OrdList elem
insert x (Ord []) = Ord [x]
insert x (Ord (y:ys))
  | x <= y = Ord (x:y:ys)
  | x > y  = Ord (y : xs)
    where (Ord xs) = insert x (Ord ys)

instance (Ord elem) => Monoid (OrdList elem) where
  mempty = Ord []
  mappend xs (Ord []) = xs
  mappend (Ord []) ys = ys
  mappend (Ord (x:xs)) ys = mappend (Ord xs) (insert x ys)

--foldm :: (a -> a -> a) -> a -> ([a] -> a)

kpg :: (Bit, Bit) -> (Carry -> Carry)
kpg (O,  O  )  =  \ _c  -> O  -- kill
kpg (O,  I  )  =  \ c   -> c  -- propagate
kpg (I,  O  )  =  \ c   -> c  -- propagate
kpg (I,  I  )  =  \ _c  -> I  -- generate

data KPG  =  K | P | G
