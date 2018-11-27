module Main
where
import System.Random
import System.Environment
import Data.Char

nextRand :: IO Int
nextRand = getStdRandom random

shift :: (Int -> Int -> Int) -> Char -> Int -> Char
shift op c r
    | a < 32  = c
    | a >= 32 = chr $ ((a - 32 `op` r) `mod` (128 - 32)) + 32
        where a = ord c

encrypt :: Char -> Int -> Char
encrypt = shift (+)

decrypt :: Char -> Int -> Char
decrypt = shift (-)

processFile :: (Char -> Int -> Char) -> String -> String -> IO ()
processFile op inFile outFile = do
    content <- readFile inFile
    processed <- mapM (\c -> do r <- nextRand; return $ op c r) content
    writeFile outFile processed

encryptFile :: String -> String -> IO ()
encryptFile = processFile encrypt

decryptFile :: String -> String -> IO ()
decryptFile = processFile decrypt

main :: IO ()
main = do
    setStdGen (mkStdGen 2144)
    args <- getArgs
    let operation = args !! 0
    let inFile = args !! 1
    let outFile = args !! 2
    case operation of
        "encrypt" -> encryptFile inFile outFile
        "decrypt" -> decryptFile inFile outFile
        otherwise -> putStrLn ("Unknown operation: " ++ operation)
