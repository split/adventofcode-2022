module Main where

import Control.Monad (ap)
import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Set qualified as S

type Point = (Int, Int, Int)

main = interact (unlines . sequence [part1] . parse)

part1 :: Set Point -> String
part1 p = ("Part 1: " ++) . show $ S.foldr ((+) . length . (S.\\ p) . sides) 0 p

sides :: Point -> Set Point
sides (x, y, z) = S.fromList [(x, y, z + 1), (x, y, z - 1), (x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z)]

parse = S.fromList . map point . lines

point = (\[x, y, z] -> (x, y, z)) . map read . splitOn ","