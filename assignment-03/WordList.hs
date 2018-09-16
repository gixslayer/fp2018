module WordList
where
import Prelude hiding (Word)
import Data.List
import Data.Char

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

-- Formatting the output so that one entry per line is shown.
-- putStr $ concat $ map (\(w,f) -> w ++ " (" ++ show f ++ ")\n") $ wordList lorem