module Main where

import Data.List.Split (splitOn)

main = interact (unlines . sequence [part1, part2] . map parse . lines)

part1 = ("Part 1: " ++) . show . length . concatMap common

part2 = ("Part 2: " ++) . show . length . concatMap some

common ([l, u] : rest) = filter (\[l2, u2] -> l2 >= l && u2 <= u || l >= l2 && u <= u2) rest

some ([l, u] : rest) = filter (\[l2, u2] -> min u u2 - max l l2 >= 0) rest

parse :: [Char] -> [[Int]]
parse = map (map read . splitOn "-") . splitOn ","