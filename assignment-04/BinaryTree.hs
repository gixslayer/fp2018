module BinaryTree
where

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
-- TODO: Compute running time (probably depends on ++ and : running time).
preorder :: Tree elem -> [elem]
preorder Empty = []
preorder (Node l e r) = e : preorder l ++ preorder r

inorder :: Tree elem -> [elem]
inorder Empty = []
inorder (Node l e r) = inorder l ++ [e] ++ inorder r

postorder :: Tree elem -> [elem]
postorder Empty = []
postorder (Node l e r) = postorder l ++ postorder r ++ [e]

-- 4.2.2
layoutBranch :: (Show elem) => Tree elem -> Int -> Bool -> [String]
layoutBranch Empty _ _ = []
layoutBranch (Node l e r) i d = (layoutBranch l (i+1) True) ++ ((br ++ show e) : (layoutBranch r (i+1) False))
    where br = if d then "/ " else "\\ "

layout :: (Show elem) => Tree elem -> String
layout Empty = "- "
layout (Node l e r) = concat $ layoutBranch l 1 True ++ "- " ++ show e ++ layoutBranch r 1 False

--build :: [elem] -> Tree elem
--balanced :: [elem] -> Tree elem
--create :: Int -> Tree ()
