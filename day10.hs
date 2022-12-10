module Main where

import Data.List (intercalate)
import Data.List.Split (chunksOf)

main = interact (unlines . sequence [part1, part2] . foldl exec [1, 1] . map words . lines)

part1 = ("Part 1: " ++) . show . sum . signals [20, 60, 100, 140, 180, 220]

part2 = intercalate "\n" . map ("Part 2: " ++) . draw . drop 1 . init

signals :: [Int] -> [Int] -> [Int]
signals ix xs = map (\i -> i * (xs !! i)) ix

draw :: [Int] -> [String]
draw = chunksOf 40 . zipWith pixel [0 ..]

pixel :: Int -> Int -> Char
pixel i x
  | abs (x - i `mod` 40) < 2 = '#'
  | otherwise = '.'

exec :: [Int] -> [String] -> [Int]
exec xs ["noop"] = xs ++ [last xs]
exec xs ["addx", s] = let x = last xs in xs ++ [x, x + read s]
exec _ _ = error "Unknown command"