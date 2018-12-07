module Generate
where
import Control.Monad

bools :: [Bool]
bools = pure False ++ pure True -- = [False, True]

maybes :: [elem] -> [Maybe elem]
maybes elems = pure Nothing ++ (Just <$> elems) -- = Nothing : map Just elems

data Suit = Spades | Hearts | Diamonds | Clubs
  deriving Show
data Rank = Faceless Integer | Jack | Queen | King -- Why Integer??
  deriving Show
data Card = Card Rank Suit | Joker
  deriving Show

suits :: [Suit]
suits = pure Spades ++ pure Hearts ++ pure Diamonds ++ pure Clubs

ranks :: [Rank]
ranks = pure Jack ++ pure Queen ++ pure King ++ (Faceless <$> [2..10])

cards :: [Card]
cards = pure Joker ++ (Card <$> ranks <*> suits)


data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
  deriving Show

list, list' :: [elem] -> Int -> [[elem]]
list = flip replicateM -- It works, don't touch it

-- list expands to:
list' elems count = foldr (\xs yss -> [x : ys | x <- xs, ys <- yss]) [[]] (replicate count elems)
{-
  For each list xs in the arguments for the cartesian product:
    For each elem x of list xs:
      For each list ys in the accumulator list yss:
        Append elem x to list ys

    So basically: concatenate each elem to all lists of the accumulator,
      and the resulting list becomes the new accumulator
-}


trees, trees' :: [comb] -> Int -> [Tree comb]
-- Simple version:
trees' _ 0 = [Empty]
trees' combs 1 = [Node Empty comb Empty | comb <- combs]
trees' combs depth =
  [Node Empty comb tree  | tree  <- subTrees, comb <- combs] ++
  [Node treeL comb treeR | treeL <- subTrees, treeR <- subTrees, comb <- combs] ++
  [Node tree  comb Empty | tree  <- subTrees, comb <- combs]
  where subTrees = trees' combs (depth-1)

-- But who needs readability?!
-- Haskell activate!!
trees _ 0 = pure Empty
trees combs 1 = flip (Node Empty) Empty <$> combs
trees combs depth = --TODO order?
  (Node Empty <$> combs <*> subTrees) ++
  (Node <$> subTrees <*> combs <*> subTrees) ++
  (Node <$> subTrees <*> combs <*> pure Empty)
  where subTrees = trees' combs (depth-1)
-- OK this even looks sort-of reasonable (but note that the order is different)
