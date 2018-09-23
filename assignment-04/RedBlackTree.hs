module RedBlackTree
where
import QuickTest

data RedBlackTree elem
  =  Leaf
  |  Red    (RedBlackTree elem) elem (RedBlackTree elem)
  |  Black  (RedBlackTree elem) elem (RedBlackTree elem)
  deriving (Show)

instance Functor RedBlackTree where
  fmap _ (Leaf)         =  Leaf
  fmap f (Red l a r)    =  Red (fmap f l) (f a) (fmap f r)
  fmap f (Black l a r)  =  Black (fmap f l) (f a) (fmap f r)

-- 4.5.1
member :: (Ord elem) => elem -> RedBlackTree elem -> Bool
member _ Leaf = False
member x (Red l e r)
  | x == e = True
  | x > e  = member x r
  | x < e  = member x l
member x (Black l e r)
  | x == e = True
  | x > e  = member x r
  | x < e  = member x l

-- 4.5.2
-- Special case for tree root, which must remain black. If the insertion
-- produces a red root, just change it to black, as that cannot violate any of
-- the conditions.
insert :: (Ord elem) => elem -> RedBlackTree elem -> RedBlackTree elem
insert x t = case insert2 x t of
  Red l e r -> Black l e r
  Black l e r -> Black l e r

insert2 :: (Ord elem) => elem -> RedBlackTree elem -> RedBlackTree elem
insert2 x Leaf = Red Leaf x Leaf
insert2 x (Red l e r)
  | x <= e = Red (insert2 x l) e r
  | x > e  = Red l e (insert2 x r)
insert2 x (Black l e r)
  | x <= e = black (insert2 x l) e r
  | x > e  = black l e (insert2 x r)

insertAll :: (Ord elem) => [elem] -> RedBlackTree elem -> RedBlackTree elem
insertAll xs t = foldr insert t xs

-- the 4 illegal shapes
-- Black (Red (Red Leaf 'a' Leaf) 'b' Leaf) 'c' Leaf
-- Black (Red Leaf 'a' (Red Leaf 'b' Leaf)) 'c' Leaf
-- Black Leaf 'a' (Red (Red Leaf 'b' Leaf) 'c' Leaf)
-- Black Leaf 'a' (Red Leaf 'b' (Red Leaf 'c' Leaf))

black :: RedBlackTree elem -> elem -> RedBlackTree elem -> RedBlackTree elem
black (Red (Red t a u) b v) c w = Red (Black t a u) b (Black v c w)
black (Red t a (Red u b v)) c w = Red (Black t a u) b (Black v c w)
black t a (Red u b (Red v c w)) = Red (Black t a u) b (Black v c w)
black t a (Red (Red u b v) c w) = Red (Black t a u) b (Black v c w)
black l x r = Black l x r

-- 4.5.3
checkColors :: RedBlackTree elem -> Bool
checkColors (Red (Red _ _ _) _ _) = False
checkColors (Red _ _ (Red _ _ _)) = False
checkColors Leaf = True
checkColors (Black l _ r) = checkColors l && checkColors r
checkColors (Red l _ r) = checkColors l && checkColors r

blackDepths :: Int -> RedBlackTree elem -> [Int]
blackDepths n Leaf = [n]
blackDepths n (Red l _ r) = blackDepths n l ++ blackDepths n r
blackDepths n (Black l _ r) = blackDepths (n+1) l ++ blackDepths (n+1) r

checkDepths :: RedBlackTree elem -> Bool
checkDepths t = and $ map (==(head depths)) depths
  where depths = blackDepths 0 t

isRedBlackTree :: RedBlackTree elem -> Bool
isRedBlackTree Leaf = False
isRedBlackTree (Red _ _ _) = False
isRedBlackTree t = checkColors t && checkDepths t

makeTree :: (Ord elem) => [elem] -> RedBlackTree elem
makeTree [x] = Black Leaf x Leaf
makeTree (x:xs) = insert x $ makeTree xs

makeTrees :: (Ord elem) => [elem] -> [RedBlackTree elem]
makeTrees xs = [ makeTree ys | ys <- permutations xs ]

redBlackTrees :: (Ord elem) => [elem] -> Probes (RedBlackTree elem)
redBlackTrees xs = [ t | t <- makeTrees xs, inorder t == xs]

inorder :: RedBlackTree elem -> [elem]
inorder Leaf = []
inorder (Red l e r) = inorder l ++ [e] ++ inorder r
inorder (Black l e r) = inorder l ++ [e] ++ inorder r

-- Tested with the following expressions, which all produce True
-- (redBlackTrees [1,2,3] --> isRedBlackTree) (insert 0)
-- (redBlackTrees [1,2,3] --> isRedBlackTree) (insert 1)
-- (redBlackTrees [1,2,3] --> isRedBlackTree) (insert 2)
-- (redBlackTrees [1,2,3] --> isRedBlackTree) (insert 5)
-- (redBlackTrees [1,2,3,4,5,6] --> isRedBlackTree) (insertAll [1..100])
