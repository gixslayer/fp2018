module Char
where
import Data.Char
import Data.List
import Data.Maybe

-- Ciske Harsema - s1010048
-- Steven Wallis de Vries - s1011387

equal      :: String -> String -> Bool
equal a b = map toLower a == map toLower b

isNumeral  :: String -> Bool
isNumeral = and . map isDigit

isBlank    :: String -> Bool
isBlank = and . map isSpace

fromDigit  :: Char -> Int
fromDigit = digitToInt
--fromDigit c = fromJust $ elemIndex c "0123456789"

toDigit    :: Int -> Char
toDigit = intToDigit
--toDigit n | n < 10 = "0123456789" !! n

shift      :: Int -> Char -> Char
shift n c
    | isAlpha c && isUpper c = chr(ord 'A' + (((ord c - ord 'A') + n) `mod` 26))
    | otherwise = c

msg :: String
msg = "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ JHLJBZ KPJABT HYJUBT LZA ULBAYVU"

decode :: Int -> String -> String
decode n s = map (\c -> shift n c) s

-- FABER EST SUAE QUISQUE FORTUNAE APPIUS CLAUDIUS CAECUS DICTUM ARCNUM EST NEUTRON
decode_msg :: IO ()
decode_msg = putStr $ intercalate "\n" $ map (\n -> decode n msg) [0..25]
