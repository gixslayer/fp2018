-- 5.1.1
allTrue :: [Bool] -> Bool
allTrue = foldr (&&) True

-- 5.1.2
allFalse :: [Bool] -> Bool
allFalse = not . foldr (||) False

-- 5.1.3
member :: (Eq a) => a -> [a] -> Bool
member e = foldr (\a b -> b || a==e) False

-- 5.1.4
smallest :: [Int] -> Int
smallest xs = foldl min (head xs) (tail xs)

-- 5.1.5
largest :: [Int] -> Int
largest xs = foldl max (head xs) (tail xs)

-- Since foldl has to traverse the entire list, foldr could be more efficient.
-- foldr, unlike foldl, can stop early. Foldl can also cause problems with very
-- deep call stacks leading to stack overflows, but some optimization tricks
-- exist for such tail recursion. Variants that force evaluation before making
-- the recursion also exist.
