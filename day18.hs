module Main where

import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)

data Point = P {x :: Int, y :: Int, z :: Int} deriving (Eq, Show, Ord)

main = interact (unlines . sequence [part1, part2] . parse)

part1 :: Set Point -> String
part1 p = ("Part 1: " ++) . show $ S.foldr ((+) . length . surface p) 0 p

part2 p = ("Part 2: " ++) . show $ length $ filter (`elem` wetSurfaces) surfaces
  where
    surfaces = concatMap (S.elems . surface p) $ S.elems p
    wetSurfaces = fillWithWater p $ S.fromAscList surfaces

fillWithWater p surfaces = trace (show $ (bmin, bmax)) $ fill [bmin] S.empty
  where
    bmin = foldrP1 min surfaces
    bmax = foldrP1 max surfaces
    inBounds :: Point -> Bool
    inBounds s =
      (x bmin <= x s && x s <= x bmax)
        && (x bmin <= y s && y s <= x bmax)
        && (z bmin <= z s && z s <= z bmax)
    fill [] _ = []
    fill (point : xs) water
      | point `elem` water = fill xs water
      | point `elem` surfaces = point : fill (nextPoints (p <> water)) nextWater
      | otherwise = fill (nextPoints water) nextWater
      where
        nextWater = S.insert point water
        nextPoints on = xs ++ S.toAscList (S.filter inBounds (surface on point))

surface :: Set Point -> Point -> Set Point
surface p = (S.\\ p) . sides

sides :: Point -> Set Point
sides (P x y z) = S.fromList [P x y (z + 1), P x y (z - 1), P (x + 1) y z, P (x - 1) y z, P x (y + 1) z, P x (y - 1) z]

parse = S.fromList . map point . lines

point = (\[x, y, z] -> P x y z) . map read . splitOn ","

foldrP1 :: Foldable f => (Int -> Int -> Int) -> f Point -> Point
foldrP1 f = foldr1 (\a b -> P (f (x a) (x b)) (f (x a) (x b)) (f (x a) (x b)))