module RandomAccessList
where

data Nat  =  Z | S Nat
  deriving Show

data List elem  =  Zero | Succ elem (List elem)

data Bin  =  N | O Bin | I Bin

type Pair elem = (elem, elem)

data Sequ elem
  =  Nil
  |  OCons       (Sequ (Pair elem))
  |  ICons elem  (Sequ (Pair elem))

fromNat :: Nat -> Int
fromNat Z   = 0
fromNat (S x) = fromNat x + 1

toNat :: Int -> Nat
toNat 0 = Z
toNat n = S $ toNat (n-1)

add :: Nat -> Nat -> Nat
add Z n = n
add n Z = n
add x (S y) = S (add x y)

-- 12.1.1
--unary   :: Bin  -> Nat
--binary  :: Nat  -> Bin

-- 12.1.2
--toList    ::  Sequ elem  -> List elem
--fromList  ::  List elem  -> Sequ elem
