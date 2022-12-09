module Main where

import Control.Monad (ap)
import Control.Monad.Trans.State
import Data.List (nub)
import Data.Set (Set)
import Data.Set qualified as Set

type Pos = (Int, Int)

data Dir = R | L | U | D
  deriving (Show, Read, Eq)

main = interact (unlines . sequence [part1] . concatMap parse . lines)

part1 :: [Dir] -> String
part1 = ("Part 1: " ++) . show . length . nub . map snd . scanl move ((0, 0), (0, 0))

move :: (Pos, Pos) -> Dir -> (Pos, Pos)
move (headPos, tailPos) dir
  | dist headPos' tailPos > 1 = (headPos', headPos)
  | otherwise = (headPos', tailPos)
  where
    headPos' = move' dir headPos

move' :: Dir -> Pos -> Pos
move' R (x, y) = (x + 1, y)
move' L (x, y) = (x - 1, y)
move' U (x, y) = (x, y - 1)
move' D (x, y) = (x, y + 1)

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = max (abs (x2 - x1)) (abs (y2 - y1))

parse :: String -> [Dir]
parse = ap (flip replicate . read . take 1) (read . drop 2)