{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Printf
where

data D  =  D  deriving (Show)
data F  =  F  deriving (Show)
data S  =  S  deriving (Show)

infixr 4 &
(&) :: a -> b -> (a, b)
a & b  =  (a, b)

type family Arg dir res :: *

printf :: (Format dir) => dir -> Arg dir String
printf dir = format dir id ""

class Format dir where
  format :: dir -> (String -> a) -> String -> Arg dir a

newtype (f :.: g) x = TC { fromTC :: f (g x) }
  deriving (Functor)

-- 12.4
type instance Arg D res = Int -> res
type instance Arg F res = Float -> res
type instance Arg S res = String -> res
type instance Arg String res = res
type instance Arg (a, b) res = Arg a res -> Arg b res -> res -- probably wrong

instance Format D where
  format D cont out = \i -> cont (out ++ show i)

instance Format F where
  format F cont out = \f -> cont (out ++ show f)

instance Format S where
  format S cont out = \s -> cont (out ++ show s)

instance Format String where
  format s cont out = cont (out ++ s)

instance (Format dir1,Format dir2) => Format (dir1, dir2) where
  format (d1, d2) cont out = undefined

combine :: (Functor f, Functor g) => f String -> g String -> f ( g String )
combine fs gs = fmap (\s -> fmap (\t -> s ++ t) gs) fs

--printf D 51
--printf ("I am " & D & " years old.") 51
--printf ("I am " & D & " " & S & " old.") 1 "year"
--fmt = "Color " & S & ", Number " & D & ", Float " & F
--printf fmt "purple" 4711 3.1415
