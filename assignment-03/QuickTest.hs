module QuickTest (Probes, Property, (-->), (==>))
where
import Data.List (sort)

-- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

type Probes a    =  [a]

type Property a  =  a -> Bool

infixr 1  -->, ==>

(-->)   :: Probes a -> Property b -> Property (a -> b)
(==>)   :: Probes a -> (a -> Property b) -> Property (a -> b)

probes --> prop  =  \ f -> and [ prop (f x) | x <- probes ]
probes ==> prop  =  \ f -> and [ prop x (f x) | x <- probes ]

-- 3.5.1
ordered :: (Ord a) => Property [a]
ordered [] = True
ordered (x:[]) = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

-- 3.5.2
-- There are n! permutations of a list of n elements.
removeAt :: Int -> [a] -> [a]
removeAt i xs | i < length xs = take i xs ++ drop (i+1) xs

permutations :: [a] -> Probes [a]
permutations [] = [[]]
permutations xs = [ (xs !! i) : ys | i <- [0..(length xs)-1], ys <- permutations (removeAt i xs) ]

-- 3.5.3
runs :: (Ord a) => [a] -> [[a]]
runs [x] = [[x]]
runs (x:y:xs) 
    | x <= y    = (x : head ys) : drop 1 ys
    | otherwise = [x] : ys
        where ys = runs (y:xs)

runProbes :: Probes String
runProbes = ["hello, world!\n", "test", "abcdcba"]

runProp :: Property [String]
runProp = and . map ordered

testRuns :: (String -> [String]) -> Bool
testRuns = runProbes --> runProp

-- Finally, run 'testRuns runs' and see if it produces True.
-- Alternatively, permutations [1..5] --> and . map ordered $ runs

-- 3.5.4
isqrt :: Integer -> Integer
isqrt n = loop 0 3 1
  where loop i k s  | s <= n      = loop (i + 1) (k + 2) (s + k)
                    | otherwise  = i

isIntegerSqrt :: Property (Integer -> Integer)
isIntegerSqrt = map (\n -> n * n) [0..10] ==> \x y -> y*y == x

-- Then test with isIntegerSqrt isqrt, or test with the build in sqrt as
-- isIntegerSqrt (floor . sqrt . fromIntegral)

-- 3.5.5
infixr 4 ***
(***) :: Probes a -> Probes b -> Probes (a, b)
(***) as bs = [(a,b) | a <- as, b <- bs]
-- Produces m*n elements where m = |as|, n = |bs|

niftySort :: [a] -> [a]
niftySort _xs  =  []

trustedSort :: (Ord a) => [a] -> [a]
trustedSort  =  sort
