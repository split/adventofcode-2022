module Main where

import Data.Char (ord)

main = interact (unlines . sequence [part1, part2] . map (parse . words) . lines)

part1 = ("Part 1: " ++) . show . sum . map score

part2 = ("Part 2: " ++) . show . sum . map (score . puppet)

score (a, b) = 1 + b + 3 * ((b - a + 1) `mod` 3)

puppet (a, b) = (a, (a + b - 1) `mod` 3)

parse [[a], [b]] = (ord a - ord 'A', ord b - ord 'X')