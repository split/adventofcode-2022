module Main where

import Data.List (drop, group, sort, tails)

main = interact (unlines . sequence [part1, part2] . lines)

part1 = ("Part 1: " ++) . show . start 4 . head

part2 = ("Part 2: " ++) . show . start 14 . head

start n = (+ n) . length . takeWhile ((/= n) . length . group . sort . take n) . tails