module DNA
where
import Prelude
import Data.List

-- Imported from List.lhs and renamed so that I can actually use filter from Data.List
filter2 :: (a -> Maybe b) -> ([a] -> [b])
filter2 _f []  =  []
filter2 f (a : as)  =  case f a of
  Nothing  ->      filter2 f as
  Just b   ->  b : filter2 f as

-- Nucleobases or DNA-bases are the basic building blocks of
-- deoxyribonucleic acid (DNA).

data Base  =  A | C | G | T
  deriving (Eq, Ord)

-- Adenin (A), Cytosin (C), Guanin (G) und Thymin (T).

instance Show Base where
  showsPrec _ A  =  showChar 'A'
  showsPrec _ C  =  showChar 'C'
  showsPrec _ G  =  showChar 'G'
  showsPrec _ T  =  showChar 'T'

  showList  =  foldr (.) id . map shows

base :: Char -> Maybe Base
base 'A'  =  Just A
base 'C'  =  Just C
base 'G'  =  Just G
base 'T'  =  Just T
base _    =  Nothing

type DNA      =  [Base]
type Segment  =  [Base]

dna  ::  DNA
dna  =  [A, T, G, T, A, A, A, G, G, G, T, C, C, A, A, T, G, A]

mm  ::  DNA
mm  =  filter2 base
   "ATGTAAAGGGTCCAATGACTGGAATTACTTCACAGCCCTGACACTGTGGAGAGATGGATA\
   \CCAGTTTAATTATGGACAGCTGAGAATAATCTGGGAGTCACCTGCTCTGGATATTTATCA\
   \TCATTATGAACTTCTCACCAGGCTGTGGCCCGAACACTTTCATAACGTCCCATTTGTGTT\
   \GGGCAGACATTATGATCTATACAGAACCTGCCTTGTCTGTCACCTTTGAATTTATGGATA\
   \GACAATTTAATAT\
   \GTGTTCCTGGCAGCAAAGATAATCATGGAGAGTGGAGAGAAACTAACCTTACCATTGATA\
   \GGGAAACTCTTGAAGTGTCAACTTCTCCATATTAAATCCAAGGACCAGCAGAGACGAGAA\
   \AATGAAAAGAAGATGGTAGAAGAAAGAACAAAATCAGAAAAAGACAAAGGAAAAGGGAAG\
   \TCTCCAAAGGAGAAGAAAGTTGCCAGTGCCAAGCCTGGGAAGGGAAAGAGCAAGGACCAG"

readDNA :: FilePath -> IO [Base]
readDNA path
  =  do  x <- readFile path
         return (filter2 base x)


-- This is essentially just isPrefixOf
beginsWith :: Segment -> DNA -> Bool
beginsWith [] _          = True
beginsWith (x:xs) []     = False
beginsWith (x:xs) (y:ys) = x == y && beginsWith xs ys

containsAt :: Int -> Segment -> DNA -> Bool
containsAt n ss ds = beginsWith ss (drop n ds)

containsPositions :: Segment -> DNA -> [Int]
containsPositions ss ds = filter (\n -> containsAt n ss ds) [0..(length ds - length ss)]

-- This is essentially just isInfixOf
contains :: Segment -> DNA -> Bool
contains ss ds = any (beginsWith ss) (tails ds)

-- Group contigious bases, and then throw out all non A bases. Then map the
-- list of contigious A bases to a list of lengths. Then find the longest
-- length and convert it from an Int to an Integer. 
longestOnlyAs :: DNA -> Integer
longestOnlyAs [] = 0
longestOnlyAs xs = toInteger $ maximum $ map length $ filter (\xs -> head xs == A) $ group xs

-- Mostly similar to longestOnlyAs, but with an additional filter to throw out
-- sequences of As longer than 10 before determining the longest sequence.
longestAtMostTenAs  :: DNA -> Integer
longestAtMostTenAs [] = 0
longestAtMostTenAs xs = toInteger $ maximum $ filter (<=10) $ map length $ filter (\xs -> head xs == A) $ group xs

-- If you want to test your code on a larger example, say within GHCi

-- dna <- readDNA "mm1.dna"
-- longestOnlyAs dna
