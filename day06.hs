module Main where
import Data.List (group, drop, tails, sort)

main = interact (unlines . sequence [part1, part2] . lines)

part1 =("Part 1: "++) . show . start 4 . head
part2 =("Part 2: "++) . show . start 14 . head
start n  = (+n) . length . takeWhile (/=n) . map (length . group . sort . take n) . tails