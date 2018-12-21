{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Minimax2
where
import Squiggol

-- Steven Wallis de Vries - s1011387
-- Ciske Harsema - s1010048

-- Multiway trees.
data Tree      elem     = Node  elem [Tree elem]
data TreeSlice elem ans = Slice elem [ans]
  deriving Functor

{- Derived Functor:
instance Functor (TreeSlice elem) where
  fmap f (Slice e sub) = Slice e (f <$> sub)
-}

instance Show elem => Show (Tree elem) where
  show = unlines . showTreeLines

showTreeLines :: Show elem => Tree elem -> [String]
showTreeLines (Node e sub) = show e : (('\t':) <$> (showTreeLines =<< sub))
  -- =<< works like concatMap apparently :)

instance (Show elem, Show ans) => Show (TreeSlice elem ans) where
  show (Slice e sub) = show e ++ '\n' : unlines (('\t':) <$> (lines . show =<< sub))

instance Base (TreeSlice elem) where
  type Rec (TreeSlice elem) = Tree elem
  inn (Slice e sub) = Node  e sub
  out (Node  e sub) = Slice e sub

size, depth :: Tree elem -> Integer
size  = fold (\(Slice _ sub) -> sum sub + 1)
depth = fold (\case
  (Slice _ [])  -> 1
  (Slice _ sub) -> maximum sub + 1)


type Position = (Int, Int)

moves :: Position -> [Position]
moves (x, y)
  | x == y    = moves' x
  | otherwise = moves' x ++ moves' y

moves' :: Int -> [Position]
moves' n = (\x -> (x, n - x)) <$> [1 .. n `div` 2]

gametreeOld, gametree :: (Position -> [Position]) -> Position -> Tree Position
gametreeOld m pos = Node pos $ gametreeOld m <$> m pos

-- Nice, it's even shorter
gametree m = unfold (\p -> Slice p (m p))


winningOld, losingOld,
  winningOld', losingOld',
  winningOld'', losingOld'',
  winning, losing :: Tree Position -> Bool

losingOld    (Node _ sub) = all winningOld sub
winningOld   (Node _ sub) = any losingOld  sub

losingOld'   (Node _ sub) = not $ any losingOld'  sub
winningOld'  (Node _ sub) = not $ all winningOld' sub

losingOld''  (Node _ sub) = all (not . losingOld'')  sub
winningOld'' (Node _ sub) = any (not . winningOld'') sub

winning = fold (\(Slice _ sub) -> any not sub)
losing  = fold (\(Slice _ sub) -> all not sub)
