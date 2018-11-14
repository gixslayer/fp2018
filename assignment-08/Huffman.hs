module Huffman
where
import Satellite
import Tree
import Data.List

-- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

-------------------------------------------------------------------------------

-- 8.1
-- Warm-up: constructing a frequency table.

frequencies  ::  (Ord char) => [char] -> [With Int char]
frequencies xs = map (\x -> count x xs :- x) (nub xs)
  where count x = length . filter (==x)

-------------------------------------------------------------------------------

-- 8.2
-- Constructing a Huffman tree.

-- 8.2.1
huffman :: [With Int char] -> Tree char
huffman xs = satellite $ makebranches $ map (\(f :- x) -> f :- Leaf x) (sort xs)

makebranches :: [With Int (Tree char)] -> With Int (Tree char)
makebranches [x] = x
makebranches ((fx :- x):(fy :- y):xs) = makebranches $ insert (fx + fy :- x :^: y) xs

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
encode t = concat . reverse . map (findcode cs)
  where cs = codes t

codes :: Tree char -> [(char, [Bit])]
codes t = makecodes t []

makecodes :: Tree char -> [Bit] -> [(char, [Bit])]
makecodes (Leaf c) bs = [(c, bs)]
makecodes (l :^: r) bs = makecodes l (bs++[O]) ++ makecodes r (bs++[I])

findcode :: (Eq char) => [(char, [Bit])] -> char -> [Bit]
findcode xs c = snd $ head $ filter (\(c1, bs) -> c1 == c) xs

-------------------------------------------------------------------------------

-- 8.4
-- Decoding a Huffman binary.

decode :: Tree char -> [Bit] -> [char]
decode t bs = decodechars t bs []

decodechars :: Tree char -> [Bit] -> [char] -> [char]
decodechars t [] cs = cs
decodechars t bs cs = decodechars t bs' (c:cs)
  where (c, bs') = decodechar t bs

decodechar :: Tree char -> [Bit] -> (char, [Bit])
decodechar (Leaf c) bs = (c, bs)
decodechar (l :^: r) (O:bs) = decodechar l bs
decodechar (l :^: r) (I:bs) = decodechar r bs

-------------------------------------------------------------------------------

-- Some test data.
-- Test case that yields true
-- let ct = huffman $ frequencies why in (decode ct $ encode ct why) == why

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
