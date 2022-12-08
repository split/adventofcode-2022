module Main where

import Control.Monad (ap, msum)
import Data.List (inits, tails, transpose)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

type Forest = Map (Int, Int) Int

main = interact (unlines . sequence [part1, part2] . forest . lines)

part1 forest = "Part 1: " ++ show (length $ Map.filterWithKey (visibleFromDir forest) forest)

part2 forest = "Part 2: " ++ show (maximum $ Map.mapWithKey (scenicScore forest) forest)

visibleFromDir :: Forest -> (Int, Int) -> Int -> Bool
visibleFromDir forest pos height = any (follow pos) dirs
  where
    follow pos dir =
      let pos' = add pos dir
       in case Map.lookup pos' forest of
            Just n -> (n < height) && follow pos' dir
            Nothing -> True

scenicScore :: Forest -> (Int, Int) -> Int -> Int
scenicScore forest pos height = product $ map (follow pos) dirs
  where
    follow pos dir =
      let pos' = add pos dir
       in case Map.lookup pos' forest of
            Just n -> if n < height then 1 + follow pos' dir else 1
            Nothing -> 0

forest :: [String] -> Forest
forest input = Map.fromList [((x, y), read [v]) | (y, row) <- zip [0 ..] input, (x, v) <- zip [0 ..] row]

dirs = [(1, 0), (0, 1), (-1, 0), (0, -1)]

add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
