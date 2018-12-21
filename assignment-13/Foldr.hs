{-# LANGUAGE LambdaCase #-}

module Foldr
where

-- Steven Wallis de Vries - s1011387
-- Ciske Harsema - s1010048

data List elem list = Nil | Cons elem list

fold :: (List elem ans -> ans) -> [elem] -> ans
fold alg = consume
  where consume []       = alg Nil
        consume (x : xs) = alg (Cons x (consume xs))

foldr2 :: (elem -> ans -> ans) -> ans -> [elem] -> ans
foldr2 f u = fold (\case Nil -> u; Cons x xs -> f x xs)

fold2 :: (List elem ans -> ans) -> [elem] -> ans
fold2 f = foldr (\x -> f . Cons x) (f Nil)
