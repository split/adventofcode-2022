module Main where

import Data.Char (digitToInt)

main = interact (unlines . sequence [part1] . lines)

part1 = ("Part 1: " ++) . decToSnafu . sum . map snafuToDec

-- >>> snafuToDec "1121-1110-1=0"
-- 314159265
snafuToDec :: String -> Int
snafuToDec = fst . foldr (\d (s, r) -> (s + (r * snafuDigit d), r * 5)) (0, 1)

-- >>> decToSnafu 314159265
-- "1121-1110-1=0"
decToSnafu :: Int -> String
decToSnafu n
  | n <= 0 = ""
  | n `mod` 5 == 4 = decToSnafu ((n `div` 5) + 1) ++ "-"
  | n `mod` 5 == 3 = decToSnafu ((n `div` 5) + 1) ++ "="
  | otherwise = decToSnafu (n `div` 5) ++ show (n `mod` 5)

snafuDigit '-' = -1
snafuDigit '=' = -2
snafuDigit d = digitToInt d
