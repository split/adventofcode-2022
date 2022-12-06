module Main where

import Data.List (drop, group, sort, tails)

main = interact (unlines . sequence [part1, part2])

part1 = ("Part 1: " ++) . show . start 4

part2 = ("Part 2: " ++) . show . start 14

start n = (+ n) . length . takeWhile ((/= n) . length . group . sort . take n) . tails