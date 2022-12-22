module Main where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Char (isDigit)
import Data.Foldable (foldl', minimumBy)
import Data.Function (on)
import Data.Functor (($>))
import Data.List (scanl', unfoldr)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Tuple (swap)
import Debug.Trace (trace)

data Dir = R | D | L | U deriving (Show, Read, Eq, Bounded, Enum)

data PathN = RotateR | RotateL | Move Int deriving (Show, Eq)

type Board = Map Point Char

type Point = (Int, Int)

main = interact (unlines . sequence [part1] . parse)

part1 = ("Part 1: " ++) . show . uncurry score . uncurry follow

follow :: Board -> [PathN] -> (Dir, Point)
follow board a =
  let path = scanl' action (R, start) a
   in trace (show start ++ "\n" ++ drawBoard board path start) $ last path
  where
    start = minimumBy (compare `on` swap) $ M.keys board

    action (dir, point) RotateL = trace (show point ++ " rotating left: " ++ show (prev dir)) (prev dir, point)
    action (dir, point) RotateR = trace (show point ++ " rotating right: " ++ show (next dir)) (next dir, point)
    action (dir, point) (Move n) = trace (show point ++ " tryig to move " ++ show n ++ " " ++ show dir) (dir, move dir n point)

    move :: Dir -> Int -> Point -> Point
    move dir 0 point = point
    move dir n point = maybe point (move dir (n - 1)) $ findNext point dir

    findNext point dir = do
      (tile, point) <- nextTile point dir <|> telepoint point dir
      trace ("  " ++ show point ++ if tile == '.' then " moving to empty spot" else " rock was hit") $ guard (tile == '.')
      return point

    nextTile point dir = tile (add point dir)
    telepoint point dir = last . takeWhile isJust . map tile $ iterate (`add` opposite dir) point
    tile point = (,point) <$> board M.!? point

-- teleport point dir = do

add :: Point -> Dir -> Point
add (x, y) R = (x + 1, y)
add (x, y) L = (x - 1, y)
add (x, y) U = (x, y - 1)
add (x, y) D = (x, y + 1)

parse = ap ((,) . parseMap . lines . head) (parsePath . head . lines . (!! 1)) . splitOn "\n\n"

parseMap :: [String] -> Map (Int, Int) Char
parseMap input = M.fromList [((x, y), v) | (y, row) <- zip [1 ..] input, (x, v) <- zip [1 ..] row, v /= ' ']

parsePath :: String -> [PathN]
parsePath ('R' : xs) = RotateR : parsePath xs
parsePath ('L' : xs) = RotateL : parsePath xs
parsePath move@(d : _) = let (n, xs) = span isDigit move in Move (read n) : parsePath xs
parsePath _ = []

next dir
  | dir == maxBound = minBound
  | otherwise = succ dir

prev dir
  | dir == minBound = maxBound
  | otherwise = pred dir

opposite dir = next (next dir)

score dir (x, y) = 1000 * y + 4 * x + length (takeWhile (/= dir) (iterate next R))

drawBoard :: Board -> [(Dir, Point)] -> Point -> String
drawBoard board path start = unlines [[drawTile (x, y) | x <- [1 .. mx]] | y <- [1 .. my]]
  where
    mx = maximum $ map fst $ M.keys board
    my = maximum $ map snd $ M.keys board
    pm = M.fromList $ map swap path

    drawTile :: (Int, Int) -> Char
    drawTile p = if p == start then 'S' else fromMaybe ' ' $ head . show <$> pm M.!? p <|> board M.!? p
