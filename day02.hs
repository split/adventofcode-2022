module Main where

import Data.Char (ord)

main = interact (unlines . sequence [part1, part2] . map (parse . words) . lines)

part1 = ("Part 1: " ++) . show . sum . map score

part2 = ("Part 2: " ++) . show . sum . map (score . puppet)

score (a, b) = b + 3 * ((b - a) `mod` 3)

puppet (a, b) = (a, (a + b - 2) `mod` 3 + 1)

parse [[a], [b]] = (ord a - ord 'A', ord b - ord 'X' + 1)