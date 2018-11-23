module Stream
where
import Prelude hiding (head, tail, repeat, map, zip, take, sum)

-- Steven Wallis de Vries - s1011387
-- Ciske Harsema - s1010048

data Stream elem  =  Cons { head :: elem, tail :: Stream elem }

-- 9.3.2
instance (Num elem) => Num (Stream elem) where
  (+) = zip (+)
  (-) = zip (-)
  (*) = zip (*)
  abs = map abs
  signum = map signum
  fromInteger x = repeat $ fromInteger x

instance (Show elem) => Show (Stream elem) where
  show s = show (take 100 s) ++ "..."
  -- Better but without take: show s = show (head s) ++ ", " ++ show (tail s)

infixr 5 <<
(<<) :: elem -> Stream elem -> Stream elem
a << s = Cons a s

from :: Integer -> Stream Integer
from n = n << from (n + 1)

-- 9.3.1
repeat :: a -> Stream a
repeat x = x << repeat x

map :: (a -> b) -> Stream a -> Stream b
map f s = f (head s) << map f (tail s)

zip :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zip f s1 s2 = f (head s1) (head s2) << zip f (tail s1) (tail s2)

-- 9.3.2
nat, fib :: Stream Integer
nat = 0 << nat + 1 -- ==from 0
fib = 0 << 1 << fib + tail fib

-- 9.3.3
take :: Integer -> Stream elem -> [elem]
take 0 _ = []
take n s = head s : take (n - 1) (tail s)

-- 9.3.4
diff :: (Num elem) => Stream elem -> Stream elem
diff s = tail s - s

sum :: (Num elem) => Stream elem -> Stream elem
sum s = 0 << accumSum s 0

accumSum :: (Num elem) => Stream elem -> elem -> Stream elem
accumSum s n = acc << accumSum (tail s) acc
  where acc = head s + n

-- sum (3*nat^2 + 3*nat + 1) == nat^3
-- sum fib == let fab = 0 << 0 << fab + tail fab + 1 in fab
