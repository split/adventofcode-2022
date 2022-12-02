module Main where

import Data.List (sort)
import Data.List.Split (splitOn)

main = interact (unlines . sequence [part1, part2])

part1 = ("Part 1: " ++) . show . maximum . map (sum . map read . lines) . splitOn "\n\n"

part2 = ("Part 2: " ++) . show . sum . take 3 . reverse . sort . map (sum . map read . lines) . splitOn "\n\n"
