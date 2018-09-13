import Control.Monad.Fix

-- 2.6.1
ex_1a :: Int -> Int
ex_1a = succ

ex_1b :: a -> a
ex_1b = id

ex_1c :: (Int, Int) -> Int
ex_1c (a, b) = a + b

ex_1d :: (a, a) -> a
ex_1d (a, b) = b

ex_1e :: (a, b) -> a
ex_1e (a, b) = a

-- How many functions of type 'Int -> Int'?
-- Since Int is a 64bit integer (on my implementation), 2^64 * 2^64 = 2^128?
-- How many functions of type 'a -> a'?
-- Only one, namely the 'id' function.

-- 2.6.2
ex_2a :: (a, a) -> (a, a)
ex_2a (a, b) = (a, b)

ex_2b :: (a, b) -> (b, a)
ex_2b (a, b) = (b, a)

ex_2c :: (a -> b) -> a -> b
ex_2c f a = f a

ex_2d :: (a, x) -> a
ex_2d (a, x) = a

ex_2e :: (x -> a -> b, a, x) -> b
ex_2e (f, a, x) = f x a

ex_2f :: (a -> b, x -> a, x) -> b
ex_2f (f, g, x) = f . g $ x

ex_2g :: (x -> a -> b, x -> a, x) -> b
ex_2g (f, g, x) = f x (g x)

-- 2.6.3
ex_3a :: Int -> (Int -> Int)
ex_3a n = (*) n

ex_3b :: (Int -> Int) -> Int
ex_3b f = f 0

ex_3c :: a -> (a -> a)
ex_3c a = \x -> a

ex_3d :: (a -> a) -> a
ex_3d = fix

-- How many functions of type '(Int -> Int) -> Int'?
-- 2^128 * 2^64 = 2^192?
-- How many functions of type '(a -> a) -> a'?
-- Only one, namely the 'fix' function.
