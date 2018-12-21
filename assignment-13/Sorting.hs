{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Sorting
where
import Squiggol

-- Steven Wallis de Vries - s1011387
-- Ciske Harsema - s1010048

data Tree elem      = Empty | Node (Tree elem) elem (Tree elem)
data TREE elem tree = EMPTY | NODE tree        elem tree
  deriving Functor

{-instance Functor (TREE elem) where
  fmap _ EMPTY        = EMPTY
  fmap f (NODE l a r) = NODE (f l) a (f r)-}

instance Show elem => Show (Tree elem) where
  show = unlines . showTreeLines

showTreeLines :: Show elem => Tree elem -> [String]
showTreeLines Empty = ["."]
showTreeLines (Node s1 e s2) = show e : (('\t':) <$> (showTreeLines =<< [s1, s2]))

{- Translation: in the slides -> here
Heap -> TREE; except that TREE should be a binary search tree
  Null -> EMPTY
  Heap elem tree tree -> NODE tree elem tree
LIST -> List
  NIL -> Nil
  CONS -> Cons
pile -> sprout; except that sprout generates a search tree and not a heap
sift -> wither; except that wither uses a search tree and not a heap
heapSort -> flatten2 . grow2; except the difference in sprout & wither
mingleSort -> flatten1 . grow1; except the difference in sprout & wither
-}

instance Base (TREE elem) where
  type Rec (TREE elem) = Tree elem
  inn EMPTY        = Empty
  inn (NODE l a r) = Node l a r
  out Empty        = EMPTY
  out (Node l a r) = NODE l a r

-- Growing a search tree.

grow1 :: Ord elem => [elem] -> Tree elem
grow1 = unfold $ para (fmap (id ▿ inn) . sprout)

grow1Alg :: Ord elem => List elem ([elem] × TREE elem [elem]) -> TREE elem [elem]
grow1Alg listSlice = id ▿ inn <$> sprout listSlice
--inn :: List elem [elem] -> [elem]
--sprout :: List elem ([elem] × TREE elem [elem]) -> TREE elem ([elem] + List elem [elem])

--was para :: (List elem ([elem] × TREE elem [elem]) -> TREE elem [elem]) -> [elem] -> TREE elem [elem]
grow1CoAlg :: Ord elem => [elem] -> TREE elem [elem]
grow1CoAlg l = grow1Alg $ id ▵ grow1CoAlg <$> out l

--was unfold :: ([elem] -> TREE elem [elem]) -> [elem] -> Tree elem
grow1' :: Ord elem => [elem] -> Tree elem
grow1' l = inn $ grow1' <$> grow1CoAlg l


grow2 :: Ord elem => [elem] -> Tree elem
grow2 = fold $ apo (sprout . fmap (id ▵ out))

grow2CoAlg :: Ord elem => List elem (Tree elem) -> TREE elem (Tree elem + List elem (Tree elem))
grow2CoAlg listSlice = sprout $ id ▵ out <$> listSlice
--out :: Tree elem -> TREE elem (Tree elem)
--sprout :: List elem (Tree elem × TREE elem (Tree elem)) -> TREE elem (Tree elem + List elem (Tree elem))

--was apo
grow2Alg :: Ord elem => List elem (Tree elem) -> Tree elem
grow2Alg listSlice = inn $ id ▿ grow2Alg <$> grow2CoAlg listSlice

--was fold
grow2' :: Ord elem => [elem] -> Tree elem
grow2' l = grow2Alg $ grow2' <$> out l
--out :: [elem] -> List elem [elem]

{-I hope this is enough explanation; this took a long time:

  grow1 each iteration applies para (grow1CoAlg) to the remaining lists to establish the next 'top' tree nodes
    para (grow1CoAlg) each iteration strips one List slice from the list (with out) and
      determines for the following elements in the List the result of applying and not applying grow1CoAlg again (or: finding a root element for that subtree)
        Then sprout is applied on the resulting List slice, which inserts the element from the slice using the following TREE slice;
          then it converts the following List in the TREE slice to a normal list
    Finally the TREE slice is converted to a normal Tree

  grow2 peels one slice of the list into a List and applies itself to the rest of the list to obtain a partial tree
    It then applies apo (grow2Alg) to that 'List elem (Tree elem)' to merge the next element into the Tree
      grow2Alg first applies grow2CoAlg to the slice
        grow2CoAlg computes the result of peeling off one more slice from the Tree in the slice, and not doing that,
          and then passes the result to sprout, which then tries to insert the element into the tree,
            but if that does not succeed, it returns Cont with a TREE slice with a new List slice with another sub Tree in which the element should be inserted
        If the result was a Cont, grow2Alg calls itself on the new List slice
        Finally it converts the TREE slices to a normal Tree using inn

  In short:
    grow1 grows the tree from top down, one slice on each iteration;
      to choose a root element for the remaining slices it uses para, which each iteration uses sprout; how exactly remains vague to me,
      please enlighten me!
    grow2 merges each element with fold into the tree up to now;
      the merging itself is done using apo, which uses Cont to choose subtrees to insert the element in (until we encounter two Stops)
-}

sprout :: Ord a => List a (x × TREE a x) -> TREE a (x + List a x)
sprout Nil = EMPTY
sprout (Cons a (t :@ EMPTY)) = NODE (Stop t) a (Stop t) --Insert into leaf
sprout (Cons a (_ :@ NODE l b r))
  | a <= b    = NODE (Cont (Cons a l)) b (Stop r) --Insert in left subtree
  | otherwise = NODE (Stop l) b (Cont (Cons a r)) --Insert in right subtree

{-
  I'm not sure if another definition for sprout is possible.
  It seems to me that this is the only logical one, but it doesn't necessarily build balanced trees.
  We cannot switch the left and the right branch, nor can we choose which branch to insert the element in,
    as the branches are not equivalent.
  It would be better to use eg. red-black trees; but I don't think that fits in this definition?
  I'd like to hear it if I'm wrong!
-}

{-For heaps from the slides:
sprout Nil = EMPTY
sprout (Cons a (t :@ EMPTY)) = NODE (Stop t) a (Stop t)
sprout (Cons a (_ :@ NODE l b r))
  | a <= b    = NODE (Cont (Cons b r)) a (Stop l)
  | otherwise = NODE (Cont (Cons a r)) b (Stop l)-}


-- Flattening a search tree.

flatten1, flatten2, flatten1', flatten2' :: {-Ord elem =>-} Tree elem -> [elem]
flatten1  = fold $ apo  (wither . fmap (id ▵ out))
flatten1' = fold flattenSlice

flatten2  = unfold $ para (fmap (id ▿ inn) . wither)
flatten2' = unfold largest

flattenSlice :: TREE elem [elem] -> [elem]
flattenSlice EMPTY        = []
flattenSlice (NODE l e r) = r ++ e : l

-- Selects the largest element and returns the new Tree seed, which is the tree with that element removed
largest :: Tree elem -> List elem (Tree elem)
largest Empty            = Nil
largest (Node l e Empty) = Cons e l
largest (Node l e r)     = Cons er (Node l e r')
  where Cons er r' = largest r

{-
  flatten1 is identical to grow2 except that it uses wither instead of sprout
    fold flattens the tree bottom-up using apo
      the alg containing apo has type 'TREE elem [elem] -> [elem]' where the subresults are the already flattened subtrees of type [elem]
  So really flatten1 can be written as a fold with a concatenation function as above

  flatten2 is identical to grow1 except that it uses wither instead of sprout
    unfold flattens the tree by each iteration selecting the largest element using para
      the coalg containing para has type 'Tree elem -> List elem (Tree elem)' and it selects the next largest element from the tree
  So really flatten2 can be written as an unfold with the largest element as above
-}

-- Flatten in descending order (like in the slides)
wither :: {-Ord a =>-} TREE a (x × List a x) -> List a (x + TREE a x)
wither EMPTY = Nil
wither (NODE (l :@ _) a (_ :@ Nil))       = Cons a (Stop l)             -- a is the largest
wither (NODE (l :@ _) a (_ :@ Cons c r')) = Cons c (Cont (NODE l a r')) -- c is the largest, continue with tree l a r'

{-For heaps from the slides:
wither EMPTY = Nil
wither (NODE (_ :@ Nil) a (r :@ _)) = Cons a (Stop r)
wither (NODE (l :@ _) a (_ :@ Nil)) = Cons a (Stop l)
wither (NODE (l :@ Cons b l') a (r :@ Cons c r'))
  | b <= c    = Cons a (Cont (NODE l' b r))
  | otherwise = Cons a (Cont (NODE l c r'))-}


-- Sorting algorithms:
sort11, sort12, sort21, sort22 :: Ord elem => [elem] -> [elem]
sort11 = flatten1 . grow1
sort12 = flatten2 . grow1
sort21 = flatten1 . grow2
sort22 = flatten2 . grow2

{-
  Algorithms using flatten1 (sort11 & sort21) resemble quick sort due to the way they flatten a tree slice:
    first they flatten/sort the left and right halves, then they combine them with the pivot element.
    The important difference is, though, that the pivot is not chosen to be the middle element in any of the grow functions
      due to the limitations of sprout, which as already stated above is often bad for performance
      because it might produces unbalenced trees, eg. growX [1..10]

  Algorithms using flatten2 (sort12 & sort22) sort-of resemble selection sort (or tree sort, as it seems: https://en.wikipedia.org/wiki/Tree_sort)
    as each time these selects and remove the largest element from the tree

  Not sure if there are any important differences between the grow functions..?
-}
