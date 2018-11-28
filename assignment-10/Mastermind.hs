module Mastermind
where
import System.Random
import Control.Monad
import Data.List
import Text.Read

type CodeWord = [Code]
data Code = White | Silver | Green | Red | Orange | Pink | Yellow | Blue
    deriving(Show, Eq, Ord, Enum, Bounded, Read)

randomCode :: IO Code
randomCode = do i <- getStdRandom (randomR (0,fromEnum (maxBound::Code) - 1)); return (toEnum i::Code)

randomCodeWord :: Int -> IO CodeWord
randomCodeWord n = replicateM n $ randomCode

filterNonMatching :: CodeWord -> CodeWord -> (CodeWord, CodeWord)
filterNonMatching [] cs         = ([], cs)
filterNonMatching gs []         = (gs, [])
filterNonMatching (g:gs) (c:cs) = if g == c then (grem, crem) else (g : grem, c : crem)
    where (grem, crem) = filterNonMatching gs cs

countWrongPos :: [CodeWord] -> [CodeWord] -> Int
countWrongPos [] _ = 0
countWrongPos _ [] = 0
countWrongPos (g:gs) (c:cs)
    | head g == head c = countWrongPos gs cs + min (length g) (length c)
    | head g > head c  = countWrongPos (g:gs) cs
    | head g < head c  = countWrongPos gs (c:cs)

checkGuess :: CodeWord -> CodeWord -> (Int, Int)
checkGuess guess code = (correctPos, wrongPos)
    where (guessNonMatching, codeNonMatching) = filterNonMatching guess code
          correctPos = length code - length codeNonMatching
          wrongPos = countWrongPos (group $ sort guessNonMatching) (group $ sort codeNonMatching)

readGuess :: IO CodeWord
readGuess = do
    line <- getLine
    case readMaybe line of
        Just w  -> return w
        Nothing -> putStrLn "Invalid input, try again" >> readGuess

showHint :: CodeWord -> CodeWord -> IO ()
showHint guess code = do
    putStrLn ("Number of correct codes in the right position: " ++ show correctPos)
    putStrLn ("Number of correct codes in the wrong position: " ++ show wrongPos)
      where (correctPos, wrongPos) = checkGuess guess code

turn :: Int -> Int -> CodeWord -> IO ()
turn tries maxTries code
    | tries > maxTries = putStrLn "The code maker has won!"
    | otherwise = do
        putStr "Enter your guess: "
        guess <- readGuess
        case guess == code of
          True  -> putStrLn ("The code breaker has won in " ++ show tries ++ " attempt(s)!")
          False -> do putStrLn "Incorrect"; showHint guess code; turn (tries + 1) maxTries code

main :: IO ()
main = do
    putStr "Enter code word length: "
    codeLine <- getLine
    putStr "Enter maximum number of attempts: "
    attemptsLine <- getLine
    let codeLength = read codeLine::Int
    let maxAttempts = read attemptsLine::Int
    code <- randomCodeWord codeLength
    putStrLn $ show code
    turn 1 maxAttempts code
