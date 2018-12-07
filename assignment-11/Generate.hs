module Generate
where

-- Steven Wallis de Vries - s1011387
-- Ciske Harsema - s1010048

bools  ::  [Bool]
bools  =  pure False ++ pure True

maybes  ::  [elem] -> [Maybe elem]
maybes elems  =  pure Nothing ++ (pure Just <*> elems)

data Suit  =  Spades | Hearts | Diamonds | Clubs
  deriving Show
data Rank  =  Faceless Integer | Jack | Queen | King
  deriving Show
data Card  =  Card Rank Suit | Joker
  deriving Show

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
  deriving Show

-- 11.3.1
suits :: [Suit]
suits = pure Spades ++ pure Hearts ++ pure Diamonds ++ pure Clubs

ranks :: [Rank]
ranks = (pure Faceless <*> [2..10]) ++ pure Jack ++ pure Queen ++ pure King

cards :: [Card]
cards = (pure Card <*> ranks <*> suits) ++ pure Joker

-- 11.3.2
lists :: [a] -> Int -> [[a]]
lists xs 0 = pure []
lists xs n = pure (:) <*> xs <*> lists xs (n-1)

-- The following also works
--lists = flip replicateM
-- as lists expands to:
--lists elems count = foldr (\xs yss -> [x : ys | x <- xs, ys <- yss]) [[]] (replicate count elems)
{-
  For each list xs in the arguments for the cartesian product:
    For each elem x of list xs:
      For each list ys in the accumulator list yss:
        Append elem x to list ys

    So basically: concatenate each elem to all lists of the accumulator,
      and the resulting list becomes the new accumulator
-}

-- I believe something like this is the idea, but I'm not quite sure.
trees :: [a] -> Int -> [Tree a]
trees xs 0 = pure Empty
trees xs 1 = pure Node <*> pure Empty <*> xs <*> pure Empty
trees xs n = (pure Node <*> pure Empty <*> xs <*> trees xs (n-1))
          ++ (pure Node <*> trees xs (n-1) <*> xs <*> trees xs (n-1))
          ++ (pure Node <*> trees xs (n-1) <*> xs <*> pure Empty)

-- We also had this version
trees' :: [comb] -> Int -> [Tree comb]
trees' _ 0 = pure Empty
trees' combs 1 = flip (Node Empty) Empty <$> combs
trees' combs depth =
  (Node Empty <$> combs <*> subTrees) ++
  (Node <$> subTrees <*> combs <*> subTrees) ++
  (Node <$> subTrees <*> combs <*> pure Empty)
  where subTrees = trees' combs (depth-1)
