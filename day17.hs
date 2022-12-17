{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Bool (bool)
import Data.List (scanl')
import Data.Maybe (fromMaybe)
import Data.Set (Set, foldl')
import Data.Set qualified as S
import Debug.Trace (trace)
import Text.Printf (printf)

type Point = (Int, Int)

data Sim = Sim {height :: Int, i :: Int, cave :: Set Point} deriving (Show)

initial = Sim {height = 0, i = 0, cave = caveFloor}

main = interact (unlines . sequence [part1] . cycle . map parseJet . head . lines)

part1 = ("Part 1: " ++) . show . height . (!! 2022) . (\p -> scanl' (simulate p) initial (cycle stoneShapes))

simulate :: [Point] -> Sim -> Set Point -> Sim
simulate jetPatterns Sim {..} stone =
  Sim
    { i = i',
      height = max height (1 + foldr (max . snd) 0 stoppedStone),
      cave = cave <> stoppedStone
    }
  where
    placedStone = S.map (add (2, height + 3)) stone
    (i', stoppedStone) = jetMove i placedStone

    jetMove :: Int -> Set Point -> (Int, Set Point)
    jetMove i stone =
      let jetted = fromMaybe stone (move cave stone (jetPatterns !! i))
       in case move cave jetted (0, -1) of
            Just movedStone -> jetMove (i + 1) movedStone
            Nothing -> (i + 1, jetted)

move cave stone dir =
  let newStone = S.map (add dir) stone
   in if not (collides cave newStone) then Just newStone else Nothing

collides cave stone = any outbound stone || not (null (S.intersection stone cave))

outbound (x, _) = x < 0 || x > 6

caveFloor = S.fromList [(x, -1) | x <- [0 .. 6]]

stoneShapes =
  [ -- ####
    S.fromList [(0, 0), (1, 0), (2, 0), (3, 0)],
    -- .#.
    -- ###
    -- .#.
    S.fromList [(1, 2), (0, 1), (1, 1), (2, 1), (1, 0)],
    -- ..#
    -- ..#
    -- ###
    S.fromList [(2, 2), (2, 1), (0, 0), (1, 0), (2, 0)],
    -- #
    -- #
    -- #
    -- #
    S.fromList [(0, 3), (0, 2), (0, 1), (0, 0)],
    -- ##
    -- ##
    S.fromList [(0, 1), (1, 1), (0, 0), (1, 0)]
  ]

parseJet '>' = (1, 0)
parseJet '<' = (-1, 0)
parseJet c = error ("Invalid input: " ++ [c])

add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

visualize cave h = unlines [[if (x, y) `elem` cave then '#' else '.' | x <- [0 .. 6]] | y <- reverse [0 .. h]]