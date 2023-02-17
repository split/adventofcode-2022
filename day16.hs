module Main where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, when)
import Data.Array (Array, array, bounds, (!))
import Data.Array.ST (readArray, runSTArray, thaw, writeArray)
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

type Graph = Map IV Valve

data Valve = Valve {iv :: IV, rate :: Int, tunnels :: Map IV Int} deriving (Show, Eq, Ord)

data IV = IV Char Char deriving (Eq, Ord)

instance Show IV where
  show (IV a b) = [a, b]

main = interact (unlines . sequence [part1] . map (readValve . parse . words) . lines)

part1 = ("Part 1: " ++) . show . buildGraph

buildGraph valves = fklsfj
  where
    valveMap = M.fromList $ map (iv >>= (,)) valves
    ivs = map iv valves

    vaa = floydWarshall (valveArr valves)

    fklsfj = [[vaa i j | i <- [1 .. length ivs - 1]] | j <- [1 .. length ivs - 1]]

    g i j = do
      valve <- (ivs !! i) `M.lookup` valveMap
      (ivs !! j) `M.lookup` tunnels valve

valveArr :: [Valve] -> Array (Int, Int) Int
valveArr valves = array ((0, 0), (n, n)) [((i, j), dist i j v) | (i, v) <- zip [0 ..] valves, j <- [0 .. n]]
  where
    ivs = map iv valves
    n = length ivs - 1
    dist i j v
      | i == j = 0
      | otherwise = fromMaybe 10000 $ (ivs !! j) `M.lookup` tunnels v

-- https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
floydWarshall input i j = d ! (n, i, j)
  where
    (_, (_, n)) = bounds input
    d = array ((0, 1, 1), (n, n, n)) [((k, i, j), shortest k i j) | k <- [0 .. n], i <- [1 .. n], j <- [1 .. n]]
    shortest 0 i j = input ! (i, j)
    shortest k i j = min (d ! (k - 1, i, j)) (d ! (k - 1, i, k) + d ! (k - 1, k, j))

-- bypass :: IV -> Map IV Valve -> Map IV Valve
-- bypass iv valves = M.update go iv valves
--   where
--     go valve = do
--       tunnels <- tunnel valve

-- Here starts the boring part, parsing

readValve :: [String] -> Valve
readValve (iv : rate : ivs) = Valve (readIV iv) (read rate) (M.fromList $ map ((,1) . readIV) ivs)
  where
    readIV (a : b : _) = IV a b

parse ("Valve" : valve : xs) = valve : parse xs
parse (('r' : 'a' : 't' : 'e' : '=' : rate) : xs) = init rate : parse xs
parse ("valves" : valves) = map (filter (/= ',')) valves
parse (_ : xs) = parse xs
parse [] = []

-- Not optimized, but pretty cool
-- https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
-- shortest :: (Int, Int, Int) -> (Int -> Int -> Maybe Int) -> Maybe Int
-- shortest (i, j, 0) g = g i j <|> Just 31
-- shortest (i, j, k) g = do
--   ijk <- shortest (i, j, k - 1) g
--   ikk <- shortest (i, k, k - 1) g
--   kjk <- shortest (k, j, k - 1) g
--   return $ min ijk (ikk + kjk)