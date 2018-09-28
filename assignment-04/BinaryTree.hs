module BinaryTree
where

-- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
  deriving (Show)

instance Functor Tree where
  fmap _f Empty         =  Empty
  fmap f  (Node l a r)  =  Node (fmap f l) (f a) (fmap f r)

-- 4.1.1
ex_4_1_1 :: Tree Char
ex_4_1_1 = Node (Node Empty 'a' (Node Empty 'b' Empty)) 'c' (Node (Node Empty 'd' Empty) 'f' (Node Empty 'g' Empty))

-- 4.1.2
ex1  ::  Tree Integer
ex1  =  Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))
-- see ex4_1_2_1.png for image

ex2  ::  Tree String
ex2  =  Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty
-- see ex4_1_2_2.png for image

ex3  ::  Tree Char
ex3  =  Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)
-- see ex4_1_2_3.png for image

-- 4.1.3
size :: Tree elem -> Int
size Empty = 0
size (Node l _ r) = size l + 1 + size r

-- 4.1.4
minHeight :: Tree elem -> Int
minHeight Empty = 0
minHeight (Node l _ r) = min (minHeight l + 1) (minHeight r + 1)

maxHeight :: Tree elem -> Int
maxHeight Empty = 0
maxHeight (Node l _ r) = max (maxHeight l + 1) (maxHeight r + 1)

-- 4.1.5
-- maxHeight cannot exceed size (worst case they are equal)
-- minHeight is at least floor of 2log of size+1.

-- 4.1.6
member :: (Eq elem) => elem -> Tree elem -> Bool
member e Empty = False
member e (Node l a r) = e == a || member e l || member e r

-- 4.2.1
-- Complexity O(n^2) as this essentially expands to something like the following:
-- e : [x:xs] ++ [y:ys]
-- Since the list concatination of n elements takes O(n), rather than O(1), the
-- running time of the algorithm is O(n^2), as this concatination is executed on
-- each recursive call.
preorder :: Tree elem -> [elem]
preorder Empty = []
preorder (Node l e r) = e : preorder l ++ preorder r

-- Complexity O(n^2) as this essentially expands to something like the following:
-- [x:xs] ++ [e] ++ [y:ys]
-- Concatinating the element e is not a problem, but concatinating the list [x:xs]
-- of n elements will take O(n), rather than O(1). Therefore the running time of
-- the algorithm will be O(n^2), as this concatination is executed on each recursive
-- call.
inorder :: Tree elem -> [elem]
inorder Empty = []
inorder (Node l e r) = inorder l ++ [e] ++ inorder r

-- Complexity O(n) as this essentially expands to something like the following:
-- [] ++ [x] ++ [y] ++ [] ++ [z] ++ [e]
-- Since only 1 element is appended with each list concat, this happens in
-- constant time. As all n elements appear (plus some empty leafs), the running
-- time is O(n).
postorder :: Tree elem -> [elem]
postorder Empty = []
postorder (Node l e r) = postorder l ++ postorder r ++ [e]

-- 4.2.2
layoutBranch :: (Show elem) => Tree elem -> Int -> Bool -> [String]
layoutBranch Empty _ _ = []
layoutBranch (Node l e r) i d = layoutBranch l (i+1) True ++ (indent ++ br ++ show e) : layoutBranch r (i+1) False
    where br = if d then "/ " else "\\ "
          indent = replicate (i*4) ' '

layout :: (Show elem) => Tree elem -> String
layout Empty = "- "
layout (Node l e r) = unlines $ layoutBranch l 1 True ++ ("- " ++ show e) : layoutBranch r 1 False

-- 4.3.1
build :: [elem] -> Tree elem
build [] = Empty
build (x:xs) = Node Empty x (build xs)

-- 4.3.2
balanced :: [elem] -> Tree elem
balanced [] = Empty
balanced (x:xs) = Node (balanced ls) x (balanced rs)
    where l = length xs
          (ls, rs) = splitAt (l `quot` 2) xs

-- 4.3.3
-- Since tree of size n can be transformed into a tree of size 2n+1 in one step
-- it's possible to roughly double the size with each step. Hence it's possible
-- to produce a tree of size ~2^n in n steps, which means it's possible to
-- produce a tree of size n in log(n) steps.
-- works for (2^n)-1, would require some modifications to produce exactly n
-- nodes, but this would not change the running time complexity.
doubleBranch :: Int -> Tree () -> Tree ()
doubleBranch n t 
  | n > 1  = Node (doubleBranch ((n-1) `quot` 2) t) () (doubleBranch ((n-1) `quot` 2) t)  
  | n == 1 = t
  | n == 0 = Empty

create :: Int -> Tree ()
create n = doubleBranch n (Node Empty () Empty)

-- Here is an alternative version that works with the same concept, but is much neater.
createAlt :: Int -> Tree ()
createAlt 0 = Empty
createAlt s
  | odd s = let half = createAlt $ (s - 1) `div` 2
             in Node half () half
  | otherwise = Node (createAlt (s - 1)) () Empty