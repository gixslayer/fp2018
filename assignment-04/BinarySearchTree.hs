module BinarySearchTree
where
import BinaryTree   hiding (member)
import QuickTest

-- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

registry  ::  Tree String
registry  =  Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty

-- 4.4.1
member :: (Ord elem) => elem -> Tree elem -> Bool
member _ Empty = False
member x (Node l e r)
    | e == x = True
    | e < x  = member x r
    | e > x  = member x l

-- 4.4.2
insert :: (Ord elem) => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x (Node l e r)
    | x <= e = Node (insert x l) e r
    | x > e  = Node l e (insert x r)

-- 4.4.3
deleteNode :: (Ord elem) => Tree elem -> Tree elem -> Tree elem
deleteNode Empty Empty = Empty
deleteNode Empty t = t
deleteNode t Empty = t
deleteNode lt rt = foldr insert lt (inorder rt)

delete :: (Ord elem) => elem -> Tree elem -> Tree elem
delete _ Empty = Empty
delete x (Node l e r)
    | x > e  = Node l e (delete x r)
    | x < e  = Node (delete x l) e r
    | x == e = deleteNode l r

-- 4.4.4
isLeftBranch :: (Ord elem) => elem -> Tree elem -> Bool
isLeftBranch pe Empty = True
isLeftBranch pe (Node l e r) = e <= pe && isLeftBranch e l && isRightBranch e r

isRightBranch :: (Ord elem) => elem -> Tree elem -> Bool
isRightBranch pe Empty = True
isRightBranch pe (Node l e r) = e > pe && isLeftBranch e l && isRightBranch e r

isSearchTree :: (Ord elem) => Tree elem -> Bool
isSearchTree Empty = True
isSearchTree (Node l e r) = isLeftBranch e l && isRightBranch e r

insertAll :: (Ord elem) => [elem] -> Tree elem -> Tree elem
insertAll xs t = foldr insert t xs

trees :: (Ord elem) => [elem] -> Probes (Tree elem)  -- should be defined in BinaryTree
trees xs = filter (\t -> inorder t == xs) [insertAll ys Empty | ys <- permutations xs]

-- [length (trees [1 . . i]) | i ← [0 . .]] generates the sequence n!
-- the following expressions were used to test and all produce true
--(trees [1,2,3,4,5] --> isSearchTree) (delete 0)
--(trees [1,2,3,4,5] --> isSearchTree) (delete 1)
--(trees [1,2,3,4,5] --> isSearchTree) (delete 2)
--(trees [1,2,3,4,5] --> isSearchTree) (delete 8)
--(trees [1,2,3,4,5] --> isSearchTree) (insert 0)
--(trees [1,2,3,4,5] --> isSearchTree) (insert 1)
--(trees [1,2,3,4,5] --> isSearchTree) (insert 2)
--(trees [1,2,3,4,5] --> isSearchTree) (insert 8)
