module Chain
where
import Satellite
import Tree
import qualified Data.Map as M
import Data.List
import Debug.Trace

--Costs and dimensions.

type Cost  =  Integer
type Dim   =  (Integer, Integer)

(#) :: Dim -> Dim -> With Cost Dim
(i, j) # (j', k)
  | j == j'    =  (i * j * k) :- (i, k)
  | otherwise  =  error "(#): dimensions do not match"

(<#>) :: With Cost Dim -> With Cost Dim -> With Cost Dim
(c1 :- d1) <#> (c2 :- d2)
  =  (c1 + c + c2) :- d where c :- d =  d1 # d2

-- Minimal costs.

minCost :: [Dim] -> With Cost Dim
minCost [a]  =  0 :- a
minCost as   =  minimum [ minCost bs <#> minCost cs | (bs, cs) <- split as ]

split :: [a] -> [([a], [a])]
split []        =  error "split: empty list"
split [_a]      =  []
split (a : as)  =  ([a], as) : [ (a : bs, cs) | (bs, cs) <- split as]

--minCost [(10, 30), (30, 5), (5, 60)]
--minCost [ (i, i + 1) | i <- [1 .. 3] ]
--minCost [ (i, i + 1) | i <- [1 .. 9] ]

-- 9.2.1
minimumCost   :: (size -> size -> With Cost size) -> [size] -> With Cost size
minimumCost f [a] = 0 :- a
minimumCost f as = minimum [ g f (minimumCost f bs) (minimumCost f cs) | (bs, cs) <- split as ]
  where g f (c1 :- d1) (c2 :- d2) = (c1 + c + c2) :- d where c :- d = f d1 d2

minCostMatrix as = minimumCost (#) as

-- 9.2.2
listCost :: Integer -> Integer -> With Cost Integer
listCost a b = a :- a + b

-- proportional to max of arguments, as addition might need to ripple the carry through all digits (and potentially add a new digit in front)
addCost :: Integer -> Integer -> With Cost Integer
addCost a b = m :- m -- or m+1? Can be either, eg 96+1=97, but 96+5=101
  where m = max a b

-- 9.2.3
optimalChain  :: (size -> size -> With Cost size) -> [size] -> With Cost (With size (Tree size))
optimalChain f [a] = 0 :- (a :- Leaf a)
optimalChain f as = minimum [ g f (optimalChain f bs) (optimalChain f cs) | (bs, cs) <- split as ]
  where g f (c1 :- (d1 :- t1)) (c2 :- (d2 :- t2)) = (c1 + c + c2) :- (d :- (t1 :^: t2)) where c :- d = f d1 d2

-- 9.2.4
-- Because each element of the left list has to be added to the right list, you want the left list to be as small as possible.
-- By making ++ right associative you first concat bs ++ cs in as ++ bs ++ cs, thus the left list is as small as possible.
-- First doing xs = as ++ bs would probably increase the size of the left list for xs ++ cs (unless as or bs is []), and thus
-- make the concatination more expensive.

-- 9.2.5
-- This is possible the worst code I've ever written, but I have no clue how to do this properly in Haskell.
-- Basically the idea is to make a lookup table for segments, which memorizes. Only if a segment is not yet
-- in the table is it recursively computed and then added to the table.
type Segment = (Int, Int)
type Chain size = With Cost (With size (Tree size))

optimizedSplit :: Segment -> [(Segment, Segment)]
optimizedSplit (f, l)
  | f > l = error "optimizedSplit: empty list"
  | f == l = []
  | otherwise = ((f, f), (f+1, l)) : [ ((f, bl), (cf, cl)) | ((bf, bl), (cf, cl)) <- optimizedSplit (f+1, l) ]

mapInsert :: (M.Map Segment (Chain size)) -> Segment -> Chain size -> (M.Map Segment (Chain size), Chain size)
mapInsert m s c = (M.insert s c m, c)

optimizedChain :: (size -> size -> With Cost size) -> [size] -> (Chain size)
optimizedChain f as = snd $ optimizedChain' f M.empty (0, length as - 1) as

optimizedChain' :: (size -> size -> With Cost size) -> (M.Map Segment (Chain size)) -> Segment -> [size] -> ((M.Map Segment (Chain size)), Chain size)
optimizedChain' f m (l, h) as
  | l == h = let a = as !! l in mapInsert m (l, h) (0 :- (a :- Leaf a))
  | l < h  = let (mf, xs) = foldr foldf (m, []) (optimizedSplit (l, h)) in (mf, minimum xs)
    where foldf (bs, cs) (m, xs) = (m'', g f x y : xs)
            where (m', x)  = lookup m  bs as
                  (m'', y) = lookup m' cs as
                  g f (c1 :- (d1 :- t1)) (c2 :- (d2 :- t2)) = (c1 + c + c2) :- (d :- (t1 :^: t2)) where c :- d = f d1 d2
          lookup m s as
            | M.member s m = (m, m M.! s)--trace "lookup" (m, m M.! s)
            | otherwise    = let (m', c) = optimizedChain' f m s as in mapInsert m' s c

