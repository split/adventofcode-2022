module Main where

import Control.Monad (ap)
import Data.List (nub)

type Pos = (Int, Int)

data Dir = R | L | U | D
  deriving (Show, Read, Eq)

main = interact (unlines . sequence [part1, part2] . headMoves . concatMap parse . lines)

part1 = ("Part 1: " ++) . show . length . nub . scanl1 moveTail

part2 = ("Part 2: " ++) . show . length . nub . (!! 9) . iterate (scanl1 moveTail)

headMoves = scanl move (0, 0)

moveTail :: Pos -> Pos -> Pos
moveTail tailPos@(tx, ty) (hx, hy) =
  if max (abs dx) (abs dy) > 1
    then (tx + signum dx, ty + signum dy)
    else tailPos
  where
    dx = hx - tx
    dy = hy - ty

move :: Pos -> Dir -> Pos
move (x, y) R = (x + 1, y)
move (x, y) L = (x - 1, y)
move (x, y) U = (x, y - 1)
move (x, y) D = (x, y + 1)

parse :: String -> [Dir]
parse = ap (replicate . read . drop 2) (read . take 1)