module Main where

import Control.Arrow ((&&&))
import Control.Monad (ap, guard, msum)
import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)
import Data.List (intercalate, partition, sortBy)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple (swap)
import Debug.Trace (trace)

type Point = (Int, Int)

main = interact (unlines . sequence [part1, part2] . iterate turn . (,0) . parseGrove . lines)

part1 = ("Part 1: " ++) . show . measureArea . fst . (!! 10)

part2 = ("Part 2: " ++) . show . countRounds . map fst

countRounds = (+ 1) . length . takeWhile (uncurry (/=)) . ap zip tail

-- turn :: Set Point -> Set Point
turn (grove, n) = (collisions newPoints, n + 1)
  where
    newPoints = map (ap (,) (pickDir grove n)) (S.elems grove)

    collisions [] = S.empty
    collisions ((elf, Nothing) : elfs) = S.insert elf (collisions elfs)
    collisions ((elf, move@(Just target)) : elfs) =
      let (hits, elfs') = partition ((== move) . snd) elfs
       in collisions elfs'
            <> if null hits
              then S.singleton target
              else S.fromList (elf : map fst hits)

pickDir :: Set Point -> Int -> Point -> Maybe Point
pickDir grove n elf = do
  dir <- msum (map checkDir groupedDirs)
  guard (or occupied)
  return dir
  where
    adjacents = map (add elf) dirs
    occupied = map (`S.member` grove) adjacents
    groupedDirs = take 4 $ drop n $ cycle $ groupDirs (zip adjacents occupied)
    checkDir points = if (not . any snd) points then return (fst $ head points) else Nothing

measureArea grove = trace displayGrove $ (maxx - minx + 1) * (maxy - miny + 1) - S.size grove
  where
    (minx, maxx) = (minimum &&& maximum) (S.map fst grove)
    (miny, maxy) = (minimum &&& maximum) (S.map snd grove)
    displayGrove :: String
    displayGrove = unlines [[if (x, y) `S.member` grove then '#' else '.' | x <- [minx .. maxx]] | y <- [miny .. maxy]]

-- (not . all snd)

groupDirs [n, ne, nw, s, se, sw, w, e] =
  [ [n, ne, nw],
    [s, se, sw],
    [w, nw, sw],
    [e, ne, se]
  ]

-- N, NE, NW, S, SE, SW, W, E
dirs = [(0, -1), (1, -1), (-1, -1), (0, 1), (1, 1), (-1, 1), (-1, 0), (1, 0)]

add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

parseGrove :: [String] -> Set Point
parseGrove input = S.fromList [(x, y) | (y, row) <- zip [0 ..] input, (x, v) <- zip [0 ..] row, v /= '.']