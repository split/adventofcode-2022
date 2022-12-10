module Main where

import Control.Monad (ap)
import Data.List (nub)
import Data.Set (Set)
import Data.Set qualified as Set

type Pos = (Int, Int)

data Dir = R | L | U | D
  deriving (Show, Read, Eq)

main = interact (unlines . sequence [part1] . concatMap parse . lines)

part1 :: [Dir] -> String
part1 = ("Part 1: " ++) . show . length . nub . tailMoves . headMoves

headMoves = scanl move (0, 0)

tailMoves :: [Pos] -> [Pos]
tailMoves = scanl moveTail (0, 0) . ap zip (drop 1)
  where
    moveTail tp (pp, hp) = if dist tp hp > 1 then pp else tp

move :: Pos -> Dir -> Pos
move (x, y) R = (x + 1, y)
move (x, y) L = (x - 1, y)
move (x, y) U = (x, y - 1)
move (x, y) D = (x, y + 1)

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = max (abs (x2 - x1)) (abs (y2 - y1))

parse :: String -> [Dir]
parse = ap (replicate . read . drop 2) (read . take 1)