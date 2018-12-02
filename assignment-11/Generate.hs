module Generate
where

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

-- TODO: This feels wrong, but idk what they actually want
trees :: [a] -> Int -> [Tree a]
trees xs 0 = pure Empty
trees xs 1 = pure Node <*> pure Empty <*> xs <*> pure Empty
trees xs n = (pure Node <*> pure Empty <*> xs <*> trees xs (n-1)) ++ (pure Node <*> trees xs (n-1) <*> xs <*> pure Empty)

--lists bools 1
--lists bools 2
--trees (lists bools 2) 1
--trees (lists bools 2) 2
