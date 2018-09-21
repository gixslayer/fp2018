-- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

-- Exercise 3.2
allTrue :: [Bool] -> Bool
allTrue [x]    = x
allTrue (x:xs) = x && allTrue xs

allFalse :: [Bool] -> Bool
allFalse [x]    = not x
allFalse (x:xs) = not x && allFalse xs

member :: (Eq a) => a -> [a] -> Bool
member e [] = False
member e (x:xs)
    | e == x    = True
    | otherwise = member e xs

smallest :: [Int] -> Int
smallest [x]    = x
smallest (x:xs) = if x < y then x else y
    where y = smallest xs

largest :: [Int] -> Int
largest [x]    = x
largest (x:xs) = if x > y then x else y
    where y = largest xs
