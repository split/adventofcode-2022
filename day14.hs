module Main where

import Control.Monad (ap, msum)
import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)
import Data.List (find, unfoldr)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)

main = interact (unlines . sequence [part1, part2] . parse)

part1 = ("Part 1: " ++) . show . length . iterateMaybe runSimulation

part2 = ("Part 2: " ++) . show . pred . length . takeWhile (S.notMember (500, 0)) . iterateMaybe runSimulation . getFloor
  where
    getFloor rocks = let b = bottom rocks in rocks <> pointsToLines [(-10000, b + 2), (10000, b + 2)]

runSimulation = ap (simulate (500, 0)) bottom

bottom = snd . maximumBy (compare `on` snd)

simulate :: (Int, Int) -> Set (Int, Int) -> Int -> Maybe (Set (Int, Int))
simulate rock rocks abyss = case found of
  Just newRock -> if snd newRock < abyss then simulate newRock rocks abyss else Nothing
  Nothing -> Just (S.insert rock rocks)
  where
    found = find (`S.notMember` rocks) (map (add rock) dirs)

parse :: String -> Set (Int, Int)
parse = foldr ((<>) . pointsToLines . parsePoints) S.empty . lines

pointsToLines :: [(Int, Int)] -> Set (Int, Int)
pointsToLines = S.fromList . concat . ap (zipWith connect) tail
  where
    connect start@(x1, y1) end@(x2, y2) =
      let dir = (signum (x2 - x1), signum (y2 - y1))
       in takeWhile (/= end) (iterate (add dir) start) ++ [end]

parsePoints :: String -> [(Int, Int)]
parsePoints = map (tuple . map read . splitOn ",") . splitOn " -> "

iterateMaybe f = unfoldr (fmap (\s -> (s, s)) . f)

tuple (a : b : _) = (a, b)
tuple _ = error "No things for tuple"

add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

dirs = [(0, 1), (-1, 1), (1, 1)]