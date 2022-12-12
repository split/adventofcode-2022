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
import Debug.Trace (trace)

type Point = (Int, Int)

type Grid a = Map Point a

main = interact (unlines . sequence [part1] . grid . lines)

part1 = ("Part 1: " ++) . maybe "" show . safestPathLength

start = lookupKey (ord 'S')

end = lookupKey (ord 'E')

safestPathLength grid = dijkstra g' s' e'
  where
    g' = M.insert s' (ord 'a') $ M.insert e' (ord 'z') grid
    s' = start grid
    e' = end grid

-- dijkstra :: Map Point Int -> Point -> Point -> Maybe (Int, Int)
dijkstra grid start end = dijkstra' initialHeap initialDistances
  where
    initialHeap :: Heap (Int, Point)
    initialHeap = H.singleton (0, start)
    initialDistances = M.insert start 0 (M.map (const (maxBound :: Int)) grid)
    dijkstra' heap distances = do
      ((dist, point), heap') <- H.uncons heap
      from <- M.lookup point grid
      if point == end
        then trace (show distances) return dist
        else
          let safer = M.differenceWithKey pickSafer (neighbors point distances) grid
              pickSafer p d to = if validMove from to && d > dist + 1 then Just (dist + 1) else Nothing
           in dijkstra' (toHeap safer <> heap') (safer <> distances)

validMove :: (Ord a, Num a) => a -> a -> Bool
validMove from to = to <= from + 1

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