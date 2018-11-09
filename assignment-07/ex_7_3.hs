-- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
  deriving (Show)

test_tree :: Tree Char
test_tree = Node (Node Empty 'a' (Node Empty 'b' Empty)) 'c' (Node (Node Empty 'd' Empty) 'f' (Node Empty 'g' Empty))

-- 7.3
skewed :: Int -> Tree ()
skewed 0 = Empty
skewed n = Node (skewed (n-1)) () Empty

inorder :: Tree elem -> [elem]
inorder Empty        = []
inorder (Node l a r) = inorder l ++ [a] ++ inorder r

-- specification (1)
--inorderCat t xs = inorder t ++ xs

-- 7.3.1
-- case t = Empty
-- inorderCat Empty xs = inorder Empty ++ xs -- specification (1)
--                     = [] ++ xs            -- definition of inorder
--                     = xs                  -- monoid

-- case t = (Node l a r)
-- inorderCat (Node l a r) xs = inorder (Node l a r) ++ xs          -- specification (1)
--                            = inorder l ++ [a] ++ inorder r ++ xs -- definition of inorder
--                            = inorder l ++ [a] ++ inorderCat r xs -- specification of inorderCat
--                            = inorder l ++ (a : inorderCat r xs)  -- definition of ++
--                            = inorderCat l (a : inorderCat r xs)  -- specification of inorderCat

-- result
inorderCat :: Tree elem -> [elem] -> [elem]
inorderCat Empty xs        = xs
inorderCat (Node l a r) xs = inorderCat l (a : inorderCat r xs)

-- 7.3.2
-- case 1: length $ inorder (skewed 8192)
-- case 2: length $ inorderCat (skewed 8192) []
-- case 2 is actually much faster, and uses less memory (as problematic ++ is eliminated)


-- 7.3.3 (preorder)
preorder :: Tree elem -> [elem]
preorder Empty = []
preorder (Node l e r) = e : preorder l ++ preorder r

-- specification (2)
-- preorderCat t xs = preorder t ++ xs

-- case t = Empty
-- preorderCat Empty xs = preorder Empty ++ xs -- specification (2)
--                      = [] ++ xs             -- definition of preorder
--                      = xs                   -- monoid

-- case t = (Node l a r)
-- preorderCat (Node l a r) xs = preorder (Node l a r) ++ xs          -- specification (2)
--                             = a : preorder l ++ preorder r ++ xs   -- definition of preorder
--                             = a : preorder l ++ preorderCat r xs   -- specification of preorderCat
--                             = a : preorderCat l (preorderCat r xs) -- specification of preorderCat

-- result
preorderCat :: Tree elem -> [elem] -> [elem]
preorderCat Empty xs        = xs
preorderCat (Node l a r) xs = a : preorderCat l (preorderCat r xs)
-- case 1: length $ preorder (skewed 8192)
-- case 2: length $ preorderCat (skewed 8192) []
-- case 2 is actually much faster, and uses less memory (as problematic ++ is eliminated)

-- 7.3.3 (postorder)
postorder :: Tree elem -> [elem]
postorder Empty = []
postorder (Node l e r) = postorder l ++ postorder r ++ [e]

-- specification (3)
-- postorderCat t xs = postorder t ++ xs

-- case t = Empty
-- postorderCat Empty xs = postorder Empty ++ xs -- specification (3)
--                       = [] ++ xs              -- definition of postorder
--                       = xs                    -- monoid

-- case t = (Node l a r)
-- postorderCat (Node l a r) xs = postorder (Node l a r) ++ xs            -- specification (3)
--                              = postorder l ++ postorder r ++ [a] ++ xs -- definition of postorder
--                              = postorder l ++ postorder r ++ (a:xs)    -- definition of ++
--                              = postorder l ++ postorderCat r (a:xs)    -- specification of postorderCat
--                              = postorderCat l (postorderCat r (a:xs))  -- specification of postorderCat

-- result
postorderCat :: Tree elem -> [elem] -> [elem]
postorderCat Empty xs        = xs
postorderCat (Node l a r) xs = postorderCat l (postorderCat r (a:xs))
-- case 1: length $ postorder (skewed 8192)
-- case 2: length $ postorderCat (skewed 8192) []
-- case 2 is actually much faster, and uses less memory (as problematic ++ is eliminated)

-- 7.3.4
-- Both use a base case and a recursive case. If you start with the right part of the first line, and put
-- the left part of the first line behind the last line, you show these expressions are equivalent. For
-- example, the base case of inorder becomes:
-- inorder Empty ++ xs = [] ++ xs            -- definition of inorder
--                     = xs                  -- monoid
--                     = inorderCat Empty xs -- definition of inorderCat
-- Combining the base case and the inductive/recursive case yields an inductive proof that
-- specification (1) holds.

-- 7.3.5
-- Hughes' lists use similar transformations to effectively implement functional lists, which can be
-- appended in constant time, and transformed back into an 'actual' list in linear time. This means
-- a lot of 'trivial' implementations that would be n^2 when using normal lists are flattened to n.
-- (couldn't actually find anything about this in the slides, only a comment stating 'more on this
-- later', but never mentioned again after).
