module Main where

import Control.Monad (ap)
import Data.Foldable (foldl')
import Data.List (elemIndex)
import Data.Maybe (fromJust)

main = interact (unlines . sequence [part1] . map read . lines)

part1 = ("Part 1: " ++) . show . grove . start0 . mix

grove x = sum [x !! 1000, x !! 2000, x !! 3000]

start0 = dropWhile (/= 0)

--- >>> take 7 $ mix [1,2,-3,3,-2,0,4]
-- [1,2,-3,4,0,3,-2]
mix :: [Int] -> [Int]
mix mixing =
  map (mixing !!) (cycle indices)
  where
    idx = [0 .. length mixing - 1]
    indices = foldl' (\idx' i -> move (i `indexIn` idx') (mixing !! i) idx') idx idx

move i dt xs =
  pre ++ (x : post)
  where
    t = (i + dt - 1) `mod` length xs' + 1
    (pre, post) = splitAt t xs'
    (x, xs') = pop i xs

pop i xs = let (pre, x : post) = splitAt i xs in (x, pre ++ post)

indexIn arr = fromJust . elemIndex arr
