module BinarySearchTree
where
import BinaryTree   hiding (member)
import QuickTest

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
-- TODO: Test properly, only tested for insert "Nienke" registry
insert :: (Ord elem) => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x (Node l e r)
    | x <= e = Node (insert x l) e r
    | x > e  = Node l e (insert x r)

-- 4.4.3
delete :: (Ord elem) => elem -> Tree elem -> Tree elem
delete _ Empty = Empty
delete x (Node l e r)
    | x > e  = Node l e (delete x r) 
    | x < e  = Node (delete x l) e r
    | x == e = Node ? ? r -- TODO: pull element from left branch into this node with and empty left branch and reinsert all other elements from the left branch? What if left branch is empty?

--isSearchTree :: (Ord elem) => Tree elem -> Bool
--trees :: [elem] -> Probes (Tree elem)  -- should be defined in BinaryTree
