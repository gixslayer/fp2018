runs :: (Ord a) => [a] -> [[a]]
runs [x] = [[x]]
runs (x:y:xs) 
    | x <= y    = (x : head ys) : drop 1 ys
    | otherwise = [x] : ys
        where ys = runs (y:xs)

-- Runs are usefull as each run is already sorted. Merging each run can then be
-- done efficiently in a merge-sort like manner.