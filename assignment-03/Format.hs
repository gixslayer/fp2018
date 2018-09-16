module Format
where
import Prelude hiding (Word)
import WordList (Word, lorem)

-- Take words as long as the line quota 'n' is not depleted. The function 
-- returns a pair of word lists, where the first word list is the formatted
-- line, and the second word list is the remaining words that didn't fit.
formatLine :: Int -> [Word] -> ([Word], [Word])
formatLine _ [] = ([], [])
formatLine n (w:ws)
    | wl <= n   = let (f, s) = formatLine (n - wl) ws in (w : f, s)
    | otherwise = ([], w:ws)
        where wl = length w + 1

format :: Int -> [Word] -> [[Word]]
format _ [] = []
format n ws = line : format n rem
    where (line, rem) = formatLine n ws

-- product of putStr $ unlines $ map unwords $ format 40 $ words lorem
--Lorem ipsum dolor sit amet, consetetur
--sadipscing elitr, sed diam nonumy
--eirmod tempor invidunt ut labore et
--dolore magna aliquyam erat, sed diam
--voluptua. At vero eos et accusam et
--justo duo dolores et ea rebum. Stet
--clita kasd gubergren, no sea takimata
--sanctus est Lorem ipsum dolor sit amet.
--Lorem ipsum dolor sit amet, consetetur
--sadipscing elitr, sed diam nonumy
--eirmod tempor invidunt ut labore et
--dolore magna aliquyam erat, sed diam
--voluptua. At vero eos et accusam et
--justo duo dolores et ea rebum. Stet
--clita kasd gubergren, no sea takimata
--sanctus est Lorem ipsum dolor sit amet.
