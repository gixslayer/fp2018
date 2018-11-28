module Main
where
import System.Environment
import Text.Printf

-- Steven Wallis de Vries - s1011387
-- Ciske Harsema - s1010048

-- (newlines, words, bytes)
type Count = (Int, Int, Int)

format :: Count -> String
format (l, w, b) = printf "%6d %6d %6d" l w b

addCount :: Count -> Count -> Count
addCount (l1, w1, b1) (l2, w2, b2) = (l1+l2, w1+w2, b1+b2)

main :: IO ()
main = do
    args <- getArgs
    counts <- mapM countFile args
    let totalCount = foldr addCount (0, 0, 0) counts
    putStrLn(format totalCount ++ " total")

countFile :: String -> IO Count
countFile path = do
    content <- readFile path
    let lineCount = length $ lines content
    let wordCount = length $ words content
    let byteCount = length content
    let count = (lineCount, wordCount, byteCount)
    putStrLn (format count ++ " " ++ path)
    return count
