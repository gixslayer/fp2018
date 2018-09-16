module QuickTest (Probes, Property, (-->), (==>))
where
import Data.List (sort)

type Probes a    =  [a]

type Property a  =  a -> Bool

infixr 1  -->, ==>

(-->)   :: Probes a -> Property b -> Property (a -> b)
(==>)   :: Probes a -> (a -> Property b) -> Property (a -> b)

probes --> prop  =  \ f -> and [ prop (f x) | x <- probes ]
probes ==> prop  =  \ f -> and [ prop x (f x) | x <- probes ]

-- ordered      :: (Ord a) => Property [a]
-- permutations :: [a] -> Probes [a]

isqrt :: Integer -> Integer
isqrt n = loop 0 3 1
  where loop i k s  | s <= n      = loop (i + 1) (k + 2) (s + k)
                    | otherwise  = i

-- infixr 4  ⊗
-- (⊗) :: Probes a -> Probes b -> Probes (a, b)

niftySort :: [a] -> [a]
niftySort _xs  =  []

trustedSort :: (Ord a) => [a] -> [a]
trustedSort  =  sort
