module Main where

import Data.Char (isUpper, ord)
import Data.List.Split (chunksOf)

main = interact (unlines . sequence [part1, part2] . lines)

part1 = ("Part 1: " ++) . show . sum . map (priority . complement . (\rucksack -> chunksOf (length rucksack `div` 2) rucksack))

part2 = ("Part 2: " ++) . show . sum . map (priority . complement) . chunksOf 3

complement (item : rest) = head $ filter (\item -> all (item `elem`) rest) item

priority :: Char -> Int
priority item = ord item - if isUpper item then ord 'A' - 27 else ord 'a' - 1