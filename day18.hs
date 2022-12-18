module Main where

import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)

type Point = (Int, Int, Int)

main = interact (unlines . sequence [part1, part2] . parse)

part1 :: Set Point -> String
part1 p = ("Part 1: " ++) . show $ S.foldr ((+) . length . surface p) 0 p

part2 p = ("Part 2: " ++) . show $ length $ filter (`elem` wetSurfaces) surfaces
  where
    surfaces = concatMap (S.elems . surface p) $ S.elems p
    wetSurfaces = fillWithWater p $ S.fromAscList surfaces

fillWithWater p surfaces = fill [start] S.empty
  where
    start = (minimum (S.map x surfaces), minimum (S.map y surfaces), minimum (S.map z surfaces))
    fill [] _ = []
    fill (point : xs) water
      | point `elem` water = fill xs water
      | point `elem` surfaces = point : fill (nextPoints (p <> water)) nextWater
      | otherwise = fill (nextPoints water) nextWater
      where
        nextWater = S.insert point water
        nextPoints on = xs ++ S.toAscList (S.filter (inBounds surfaces) (surface on point))

surface :: Set Point -> Point -> Set Point
surface p = (S.\\ p) . sides

sides :: Point -> Set Point
sides (x, y, z) = S.fromList [(x, y, z + 1), (x, y, z - 1), (x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z)]

parse = S.fromList . map point . lines

point = (\[x, y, z] -> (x, y, z)) . map read . splitOn ","

inBounds :: Set Point -> Point -> Bool
inBounds p (x', y', z') =
  (x' >= minimum xs && x' <= maximum xs)
    && (y' >= minimum ys && y' <= maximum ys)
    && (z' >= minimum zs && z' <= maximum zs)
  where
    xs = S.map x p
    ys = S.map y p
    zs = S.map z p

x (x', _, _) = x'

y (_, y', _) = y'

z (_, _, z') = z'