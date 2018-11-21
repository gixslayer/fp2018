module Stream
where
import Prelude hiding (head, tail, repeat, map, zip, take, sum)

data Stream elem  =  Cons { head :: elem, tail :: Stream elem }

infixr 5 <<
(<<)    ::  elem -> Stream elem -> Stream elem
a << s  =   Cons a s

from :: Integer -> Stream Integer
from n = n << from (n + 1)

--repeat  ::  a -> Stream a
--map     ::  (a -> b) -> (Stream a -> Stream b)
--zip     ::  (a -> b -> c) -> (Stream a -> Stream b -> Stream c)

--instance (Num elem) => Num (Stream elem) where

--nat, fib :: Stream Integer
--nat  =  0 << nat + 1
--fib  =  0 << 1 << fib + tail fib

--take :: Integer -> Stream elem -> [elem]

--diff :: (Num elem) => Stream elem -> Stream elem
--diff s  =  tail s - s

--sum :: (Num elem) => Stream elem -> Stream elem

