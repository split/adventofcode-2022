module Main where

import Data.Char (isUpper, ord)
import Data.List (intersect)
import Data.List.Split (chunksOf)

main = interact (unlines . sequence [part1, part2] . lines)

part1 = ("Part 1: " ++) . show . sum . map (priority . complement . (((`div` 2) . length) >>= chunksOf))

part2 = ("Part 2: " ++) . show . sum . map (priority . complement) . chunksOf 3

complement = head . foldr1 intersect

priority :: Char -> Int
priority item = ord item - if isUpper item then ord 'A' - 27 else ord 'a' - 1