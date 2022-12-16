module Main where

import Data.Char (isDigit)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (nub, sortBy)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)

type Point = (Int, Int)

main = interact (unlines . sequence [part1] . map (t2p . parse) . lines)

part1 :: [(Point, Point)] -> String
part1 = ("Part 1: " ++) . show . (ex 10 2000000 >>= scanFreq)

ex e r points = if length points <= 14 then e else r

scanFreq hz sensors = sum (map snd $ mergeSignals slices) - beacons
  where
    beacons = length $ nub $ filter ((== hz) . snd) $ map snd sensors
    slices = sortBy (compare `on` (fst . fst)) $ filter ((> 0) . snd) $ map (sliceFreq hz) sensors

tuningFreq (x, y) = x * 4000000 + y

-- xxxxxx                    xxxxxxyyyyjjj
--    yyyyyyy   Bkkk  --->                Bkkk
--         jjjjj
mergeSignals [] = []
mergeSignals (p : xs) = let xs' = mergeSignals xs in uncurry (:) $ foldl go (p, []) xs'
  where
    go (p@(s1@(x1, y1), l1), xs'') p2@(s2@(x2, y2), l2)
      | x1 <= x2 && x2 <= (x1 + l1) = let p' = (s1, max l1 (x2 - x1 + l2)) in (p', xs'')
      | otherwise = (p, p2 : xs'')

sliceFreq hz (sp@(sx, sy), bp@(bx, by)) = let d = dist sp bp - dist sp (sx, hz) in ((sx - d, hz), max 0 (2 * d + 1))

dist (sx, sy) (bx, by) = abs (sx - bx) + abs (sy - by)

--      2    1
--     212   3
--    21012  5
--     212   3
--      2    1

parse [] = []
parse chunk@(c : xs)
  | isDigit c || c == '-' = let (num, xs') = span isDigit xs in read (c : num) : parse xs'
  | otherwise = parse xs

t2p [sx, sy, bx, by] = ((sx, sy), (bx, by))