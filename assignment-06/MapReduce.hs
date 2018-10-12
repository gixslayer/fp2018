module MapReduce
where
import Prelude hiding (Monoid, mempty, mappend)
import Hardware

--- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

class Monoid a where
  mempty  :: a
  mappend :: a -> a -> a

reduce  ::  (Monoid m) => [m] -> m
reduce  =  foldr mappend mempty

-- 6.2.1
-- There are two boolean monoids.
newtype And = And {fromAnd::Bool}
  deriving (Show)

instance Monoid And where
  mempty = And True
  mappend x y = And (fromAnd x && fromAnd y)

newtype Or = Or {fromOr::Bool}
  deriving (Show)

instance Monoid Or where
  mempty = Or False
  mappend x y = Or (fromOr x || fromOr y)

-- 6.2.2
-- The reduce of And & Or are predefined as functions and & or
my_and :: [Bool] -> Bool
my_and = fromAnd . reduce . map And -- same as 'and'

my_or :: [Bool] -> Bool
my_or = fromOr . reduce . map Or -- same as 'or'

-- 6.3
newtype OrdList elem = Ord {fromOrd::[elem]}
  deriving (Show)

insert :: (Ord elem) => elem -> OrdList elem -> OrdList elem
insert x (Ord []) = Ord [x]
insert x (Ord (y:ys))
  | x <= y = Ord (x:y:ys)
  | x > y  = Ord (y : xs)
    where xs = fromOrd $ insert x (Ord ys)

instance (Ord elem) => Monoid (OrdList elem) where
  mempty = Ord []
  mappend xs (Ord []) = xs
  mappend (Ord []) ys = ys
  mappend (Ord (x:xs)) ys = mappend (Ord xs) (insert x ys)

msort :: (Ord elem) => [elem] -> [elem]
msort [] = []
msort [x] = [x]
msort xs = fromOrd $ (Ord $ msort ys) `mappend` (Ord $ msort zs)
  where (ys, zs) = splitAt (length xs `div` 2) xs

-- 6.4.1
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- foldm ::               (a -> a -> a) -> a -> ([a] -> a)
-- folm gives back accumulates to a result of the same type, because it has to work with monads
-- among other things. foldl/foldr are less restrictive

-- 6.4.2
foldmTD :: (a -> a -> a) -> a -> ([a] -> a)
foldmTD _ z []  = z
foldmTD _ _ [x] = x
foldmTD f z xs  = foldmTD f z ys `f` foldmTD f z zs
  where (ys, zs) = splitAt (length xs `div` 2) xs

-- 6.4.3
pairs :: a -> [a] -> [(a,a)]
pairs _ [] = []
pairs z [x] = [(x,z)]
pairs z (x:y:xs) = (x,y) : pairs z xs

foldmBU :: (a -> a -> a) -> a -> ([a] -> a)
foldmBU _ z []  = z
foldmBU _ _ [x] = x
foldmBU f z xs  = foldmBU f z $ map (uncurry f) $ pairs z xs

-- 6.5
-- The function way
newtype KPG = KPG (Carry -> Carry)

kpg :: (Bit, Bit) -> KPG
kpg (O, O) = KPG $ const O  -- kill
kpg (I, I) = KPG $ const I  -- generate
kpg _      = KPG id         -- propagate

instance Monoid KPG where
  mempty = KPG id
  mappend (KPG l) (KPG r) = KPG $ l . r

test0 :: Bool
test0 = f O == O
  where KPG f = reduce $ map kpg [(O,I), (I,O), (O,I)] --PPP: O

-- 6.5.1
-- The enum way
data KPG'  =  K | P | G

kpg' :: (Bit, Bit) -> KPG'
kpg' (O, O) = K
kpg' (I, I) = G
kpg' _      = P

instance Monoid KPG' where
  mempty = P
  mappend K _ = K
  mappend G _ = G
  mappend P a = a

-- 6.5.2
apply :: KPG' -> (Carry -> Carry)
apply P = id
apply K = const O
apply G = const I

test'0, test'1, test'2, test'3, test'4, test'5, test'6, test'7, testsPass' :: Bool
test'0 = apply (P `mappend` K `mappend` P) O == (apply P . apply K . apply P) I
test'1 = apply (P `mappend` K `mappend` P) O == (apply P . apply K . apply P) O
test'2 = (apply $ reduce $ map kpg' [(O,I), (I,O), (O,I)]) O == O --PPP
test'3 = (apply $ reduce [P, P, P]) I == I --I
test'4 = (apply $ reduce [P, G, P]) O == I --I
test'5 = (apply $ reduce [P, G, P]) I == I --I
test'6 = (apply $ reduce [P, K, P]) O == O --O
test'7 = (apply $ reduce [P, K, P]) I == O --O
testsPass' = and [test'0, test'1, test'2, test'3, test'4, test'5, test'6, test'7]

-- 6.5.3
-- The tuple way
newtype KPG'' = KPG'' (Bit, Bit)

instance Monoid KPG'' where
  mempty = KPG'' (O,I) -- P
  mappend (KPG'' (O,O)) _ = KPG'' (O,O) -- K
  mappend (KPG'' (I,I)) _ = KPG'' (I,I) -- G
  mappend _ a = a -- P

apply' :: KPG'' -> (Carry -> Carry)
apply' (KPG'' (O,O)) = const O -- K
apply' (KPG'' (I,I)) = const I -- G
apply' _ = id -- P

test''0, test''1, test''2, test''3, test''4, test''5, testsPass'' :: Bool
test''0 = (apply' $ reduce $ map KPG'' [(O,I), (I,O), (O,I)]) O == O --PPP
test''1 = (apply' $ reduce $ map KPG'' [(O,I), (I,O), (O,I)]) I == I --PPP
test''2 = (apply' $ reduce $ map KPG'' [(O,I), (O,O), (O,I)]) O == O --PKP
test''3 = (apply' $ reduce $ map KPG'' [(O,I), (O,O), (O,I)]) I == O --PKP
test''4 = (apply' $ reduce $ map KPG'' [(O,I), (I,I), (O,I)]) O == I --PGP
test''5 = (apply' $ reduce $ map KPG'' [(O,I), (I,I), (O,I)]) I == I --PGP
testsPass'' = and [test''0, test''1, test''2, test''3, test''4, test''5]
