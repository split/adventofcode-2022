module Main where

import Control.Monad (ap)
import Data.Char (isDigit)
import Data.List (elemIndex, findIndices, sort)
import Data.List.Split (splitOn)

data Signal a = Packet a | Signal [Signal a] deriving (Show, Read, Eq)

instance (Eq a, Ord a) => Ord (Signal a) where
  compare (Packet a) (Packet b) = a `compare` b
  compare (Signal l1) (Signal l2) = l1 `compare` l2
  compare a@(Packet _) (Signal l2) = [a] `compare` l2
  compare (Signal l1) b@(Packet _) = l1 `compare` [b]

main = interact (unlines . sequence [part1, part2] . map (map signal . lines) . splitOn "\n\n")

part1, part2 :: [[Signal Int]] -> String
part1 = ("Part 1: " ++) . show . sum . map fst . filter ((\[a, b] -> a <= b) . snd) . zip [1 ..]
part2 = ("Part 2: " ++) . show . product . map (+ 1) . findIndices (`elem` dividers) . sort . withDividers

withDividers :: [[Signal Int]] -> [Signal Int]
withDividers = (dividers ++) . concat

dividers = [signal "[[2]]", signal "[[6]]"]

signal :: Read a => String -> Signal a
signal = read . signal'

signal' :: String -> String
signal' [] = ""
signal' ('[' : xs) = "Signal [" ++ signal' xs
signal' (c : xs)
  | isDigit c = let (s, e) = span isDigit xs in "Packet " ++ c : s ++ signal' e
  | otherwise = c : signal' xs
