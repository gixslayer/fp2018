{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Squiggol
where

-- Base functor.

class Functor f => Base f where
  type Rec f :: *
  inn  ::  f (Rec f) -> Rec f  --   tying the recursive knot (merge one slice)
  out  ::  Rec f -> f (Rec f)  -- untying the recursive knot (peel off slice)

-- Fold and unfold.

fold   :: (Base f) => (f a -> a) -> Rec f -> a
fold alg = consume
  where consume = alg . fmap consume . out
--fold alg l = alg $ fold alg <$> out l

unfold :: (Base f) => (a -> f a) -> a -> Rec f
unfold coalg = produce
  where produce = inn . fmap produce . coalg
--unfold coalg l = inn $ unfold coalg <$> coalg l

-- Para- and apomorphism.

data a × b = a :@ b
(▵) :: (x -> a) -> (x -> b) -> (x -> a × b)
(f ▵ g) x = f x :@ g x

para :: (Base f) => (f (Rec f × a) -> a) -> (Rec f -> a)
para alg = consume
  where consume = alg . fmap (id ▵ consume) . out

data a + b = Stop a | Cont b
(▿) :: (a -> x) -> (b -> x) -> (a + b -> x)
(f ▿ _) (Stop a) = f a
(_ ▿ g) (Cont b) = g b

apo :: (Base f) => (a -> f (Rec f + a)) -> (a -> Rec f)
apo coalg = produce
  where produce = inn . fmap (id ▿ produce) . coalg

-- Base functor for Haskell's list datatype.

data List elem list = Nil | Cons elem list
  deriving Functor

{-instance Functor (List elem) where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons x (f xs)-}

instance Base (List elem) where
  type Rec (List elem) = [elem]
  inn Nil         = []
  inn (Cons x xs) = x : xs
  out []          = Nil
  out (x : xs)    = Cons x xs
