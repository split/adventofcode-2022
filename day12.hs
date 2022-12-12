{-# LANGUAGE PackageImports #-}

module Main where

import Control.Monad (ap)
import Data.Char (ord)
import "heaps" Data.Heap (Heap)
import "heaps" Data.Heap qualified as H
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Tuple (swap)

type Point = (Int, Int)

type Grid a = Map Point a

main = interact (unlines . sequence [part1, part2] . grid . lines)

part1 = ("Part 1: " ++) . maybe "" show . safestPathLength (==)

part2 g = "Part 2: " ++ maybe "" show (safestPathLength (\_ p -> (g M.! p) == ord 'a') g)

safestPathLength startCheck grid = dijkstra grid' end (startCheck start)
  where
    grid' = M.insert start (ord 'a') $ M.insert end (ord 'z') grid
    start = lookupKey (ord 'S') grid
    end = lookupKey (ord 'E') grid

dijkstra grid start isEnd = dijkstra' initialHeap initialDistances
  where
    initialHeap :: Heap (Int, Point)
    initialHeap = H.singleton (0, start)
    initialDistances = M.insert start 0 (M.map (const (maxBound :: Int)) grid)
    dijkstra' heap distances = do
      ((dist, point), heap') <- H.uncons heap
      from <- M.lookup point grid
      if isEnd point
        then return dist
        else
          let safer = M.differenceWith pickSafer (neighbors point distances) grid
              pickSafer d to = if validMove from to && d > dist + 1 then Just (dist + 1) else Nothing
           in dijkstra' (toHeap safer <> heap') (safer <> distances)

validMove :: (Ord a, Num a) => a -> a -> Bool
validMove from to = to >= from - 1

grid :: [String] -> Grid Int
grid rows = M.fromList [((x, y), ord col) | (cols, y) <- zip rows [0 ..], (col, x) <- zip cols [0 ..]]

neighbors :: Point -> Grid a -> Grid a
neighbors point grid = M.fromList $ mapMaybe (\a -> (a,) <$> M.lookup a grid) $ neighborPoints point

toHeap :: Grid Int -> Heap (Int, Point)
toHeap = H.fromList . map swap . M.toList

neighborPoints :: (Num a, Num b, Enum a, Enum b, Eq a, Eq b) => (a, b) -> [(a, b)]
neighborPoints (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(1, 0), (0, 1), (-1, 0), (0, -1)]]

lookupKey value = head . M.foldrWithKey go []
  where
    go key value' found
      | value == value' = key : found
      | otherwise = found