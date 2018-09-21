-- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

-- First recursively compute the runs for the remainder of the iput, and then
-- see if the element x should be appended to the first run, or inserted as a
-- new run.
runs :: (Ord a) => [a] -> [[a]]
runs [x] = [[x]]
runs (x:y:xs)
    | x <= y    = (x : head ys) : tail ys
    | otherwise = [x] : ys
        where ys = runs (y:xs)

-- Runs are usefull as each run is already sorted. Merging each run can then be
-- done efficiently in a merge-sort like manner.
