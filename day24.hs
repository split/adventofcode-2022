{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (ap)
import Data.Foldable (foldr')
import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)
import Text.Printf (printf)

data Valley = Valley {blizzards :: Set Point, start :: Point, ends :: [Point], width :: Int, height :: Int} deriving (Show)

data Point = P Int Int Dir deriving (Eq, Ord)

data Dir = R | D | L | U deriving (Eq, Ord, Enum, Bounded)

main = interact (unlines . sequence [part1, part2] . parseValley . lines)

part1 = ("Part 1: " ++) . show . journey

part2 = ("Part 2: " ++) . show . journey . withSnackRun
  where
    withSnackRun valley@Valley {..} = valley {ends = ends ++ [P 0 (-1) U, head ends]}

journey valley@Valley {..} = go [(start, 0)] S.empty ends
  where
    go ((elfs@(P x y dir), time) : xs) visited ends'@(end : nextJourneys)
      | elfs == end = if null nextJourneys then time else go [(elfs, time)] S.empty nextJourneys
      | key `S.member` visited = go xs visited ends'
      | otherwise = go (xs ++ plans) (S.insert key visited) ends'
      where
        key = (x, y, time `mod` lcm width height)
        plans = (,time + 1) <$> if safe then elfs : forecasts else forecasts
        moves = filter (inValley end) . map (walk 1 . turn elfs) $ enumFrom R
        safe = all (null . forecast time . turn elfs) (enumFrom R)
        forecasts = filter (null . forecast time) moves

    inValley end elfs@(P x y _) = elfs `inPoint` start || elfs `inPoint` end || inBounds elfs
    inBounds elfs@(P x y _) = 0 <= x && 0 <= y && x < width && y < height
    inPoint (P x1 y1 _) (P x2 y2 _) = x1 == x2 && y1 == y2
    wrapAround (P x y dir) = P (x `mod` width) (y `mod` height) dir

    forecast time elfs
      | not (inBounds elfs) = S.empty
      | otherwise =
          S.intersection
            (S.fromList $ map (turnAround . wrapAround . walk (time + 1) . turn elfs) (enumFrom R))
            blizzards

walk steps (P x y dir) = case dir of
  R -> P (x + steps) y dir
  D -> P x (y + steps) dir
  L -> P (x - steps) y dir
  U -> P x (y - steps) dir

turnAround p@(P _ _ dir) = turn p (next (next dir))

turn (P x y _) = P x y

next, prev :: (Eq a, Bounded a, Enum a) => a -> a
next dir = if dir == maxBound then minBound else succ dir
prev dir = if dir == minBound then maxBound else pred dir

-- Quite boring parsing logic starts here

parseValley :: [String] -> Valley
parseValley input =
  Valley
    { blizzards = parseBlizzards input,
      start = P 0 (-1) D,
      ends = [P (width - 1) height D],
      width,
      height
    }
  where
    height = length input - 2
    width = length (head input) - 2

parseBlizzards input =
  S.fromList
    [ P x y (read [v])
      | (y, row) <- zip [0 ..] (init $ tail input),
        (x, v) <- zip [0 ..] (init $ tail row),
        v /= '.' && v /= '#'
    ]

instance Show Point where
  show (P x y dir) = printf "(%d, %d, %s)" x y (show dir)

instance Read Dir where
  readsPrec _ ('>' : xs) = [(R, xs)]
  readsPrec _ ('v' : xs) = [(D, xs)]
  readsPrec _ ('<' : xs) = [(L, xs)]
  readsPrec _ ('^' : xs) = [(U, xs)]
  readsPrec _ _ = []

instance Show Dir where
  show R = ">"
  show D = "v"
  show L = "<"
  show U = "^"
