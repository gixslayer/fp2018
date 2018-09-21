module WordList
where
import Prelude hiding (Word)
import Data.List
import Data.Char

-- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

type Word  =  String

lorem :: String
lorem
  = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam \
    \nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam \
    \erat, sed diam voluptua. At vero eos et accusam et justo duo dolores \
    \et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est \
    \Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur \
    \sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et \
    \dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam \
    \et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea \
    \takimata sanctus est Lorem ipsum dolor sit amet."

cleanWord :: Word -> Word
cleanWord = map toLower . filter isAlphaNum

wordList :: String -> [(Word, Int)]
wordList = sortOn snd . map (\ws -> (head ws, length ws)) . group . sort . map cleanWord . words

-- Stable sorting is usually a welcome feature as it sorts identical elements
-- in the same order they appear in the input. It allows stacking of sorting
-- algorithms which would not always be the case for unstable algorithms. Since
-- the sort order of the initial sort is preserved, the output of words with
-- identical frequency is still sorted on the words (eg 'at' before 'clita').

-- Formatting the output so that one entry per line is shown.
-- putStr $ concat $ map (\(w,f) -> w ++ " (" ++ show f ++ ")\n") $ wordList lorem
