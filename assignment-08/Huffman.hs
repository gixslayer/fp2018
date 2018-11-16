module Huffman
where
import Satellite
import Tree
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as SMap

-- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

-------------------------------------------------------------------------------

-- 8.1
-- Warm-up: constructing a frequency table.

frequencies  ::  (Ord char) => [char] -> [With Int char]
frequencies = map (\g -> length g :- head g) . group . sort

-------------------------------------------------------------------------------

-- 8.2
-- Constructing a Huffman tree.

-- 8.2.1
huffman :: [With Int char] -> Tree char
huffman = satellite . collapsebranches . map (\(f :- x) -> f :- Leaf x) . sort

collapsebranches :: [With Int (Tree char)] -> With Int (Tree char)
collapsebranches [x] = x
collapsebranches ((fx :- x):(fy :- y):xs) = collapsebranches $ insert (fx + fy :- x :^: y) xs

-- 8.2.2
engfreq :: [With Int Char]
engfreq = [(8167 :- 'a'), (1492 :- 'b'), (2782 :- 'c'), (4253 :- 'd'), (12702 :- 'e'), (2228 :- 'f'), (2015 :- 'g'),
           (6094 :- 'h'), (6966 :- 'i'), (153 :- 'j'), (772 :- 'k'), (4025 :- 'l'), (2406 :- 'm'), (6749 :- 'n'),
           (7507 :- 'o'), (1929 :- 'p'), (95 :- 'q'), (5987 :- 'r'), (6327 :- 's'), (9056 :- 't'), (2758 :- 'u'),
           (978 :- 'v'), (2360 :- 'w'), (150 :- 'x'), (1974 :- 'y'), (74 :- 'z')]

-------------------------------------------------------------------------------

-- 8.3
-- Encoding ASCII text.

data Bit = O | I
  deriving (Show, Eq, Ord)

-- 8.3.1
encode :: (Eq char) => Tree char -> [char] -> [Bit]
encode t = let cm = codes t in concatMap (lookup cm)
  where lookup cm c = snd $ fromJust $ find ((==c) . fst) cm

codes :: Tree char -> [(char, [Bit])]
codes (Leaf c) = [(c, [])]
codes (l :^: r) = addpath O (codes l) ++ addpath I (codes r)
  where addpath b = map (\(c, bs) -> (c, b:bs))

-- Efficient with Map; but char has to be Ord:
encode' :: (Ord char) => Tree char -> [char] -> [Bit]
encode' t = concatMap (\c -> cm SMap.! c)
  where cm = codes' t [] SMap.empty

codes' :: (Ord char) => Tree char -> [Bit] -> SMap.Map char [Bit] -> SMap.Map char [Bit]
codes' (Leaf c) p m = SMap.insert c (reverse p) m
codes' (l :^: r) p m = codes' r (I:p) $ codes' l (O:p) m

quickEncode :: (Ord char) => [char] -> [Bit]
quickEncode s = encode' (huffman $ frequencies s) s

-------------------------------------------------------------------------------

-- 8.4
-- Decoding a Huffman binary.

decode :: Tree char -> [Bit] -> [char]
decode t [] = []
decode t bs = c : decode t bs'
  where (c, bs') = decodechar t bs

decodechar :: Tree char -> [Bit] -> (char, [Bit])
decodechar (Leaf c) bs = (c, bs)
decodechar (l :^: r) (O:bs) = decodechar l bs
decodechar (l :^: r) (I:bs) = decodechar r bs

-------------------------------------------------------------------------------

-- Some test data.
testHuffman :: (Ord char) => [char] -> Bool
testHuffman cs = ((==cs) $ decode ct $ encode ct cs) && ((==cs) $ decode ct $ encode' ct cs)
  where ct = huffman $ frequencies cs

hw, why :: String
hw = "hello world"

--code = huffman (frequencies hw)
--encode code hw
--decode code it
--decode code it == hw

why =
  "As software becomes more and more complex, it\n\
  \is  more  and  more important to structure it\n\
  \well.  Well-structured  software  is  easy to\n\
  \write,   easy   to   debug,  and  provides  a\n\
  \collection  of modules that can be re-used to\n\
  \reduce future programming costs. Conventional\n\
  \languages place a conceptual limit on the way\n\
  \problems   can   be  modularised.  Functional\n\ 
  \languages  push  those  limits  back. In this\n\
  \paper we show that two features of functional\n\
  \languages    in    particular,   higher-order\n\
  \functions and lazy evaluation, can contribute\n\
  \greatly  to  modularity.  Since modularity is\n\
  \the key to successful programming, functional\n\
  \languages  are  vitally important to the real\n\
  \world."

--code = huffman (frequencies why)
--encode code why
--decode code it
--decode code it == why
