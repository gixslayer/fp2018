module Numeral
where

-- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

type Base   =  Integer
type Digit  =  Integer

-- 5.2.1
msdf :: Base -> [Digit] -> Integer
msdf b = foldl (\x y -> x * b + y) 0

lsdf :: Base -> [Digit] -> Integer
lsdf b = foldr (\x y -> x + y * b) 0

-- 5.2.2
-- lsdf b xs = msdf b (reverse xs)
-- msdf b xs = lsdf b (reverse xs)
-- in general both foldr and foldl can be written in terms of eachother.
-- This will break with writing foldr as foldl in combination with infinite
-- lists however.
