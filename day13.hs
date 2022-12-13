module Main where

import Data.List.Split (splitOn,)
import Data.Char (isDigit)

data Signal a = Packet a | Signal [Signal a] deriving (Show, Read, Eq)

instance (Eq a, Ord a) => Ord (Signal a) where
    compare (Packet a) (Packet b) = a `compare` b
    compare (Signal l1) (Signal l2) = l1 `compare` l2
    compare a@(Packet _) (Signal l2) = [a] `compare` l2
    compare (Signal l1) b@(Packet _) = l1 `compare` [b]

main = interact (unlines . sequence [part1] . map (map signal . lines) . splitOn "\n\n")

part1 :: [[Signal Int]] -> String
part1 = ("Part 1: " ++) . show . sum . map fst . filter ((\[a, b] -> a <= b) . snd) . zip [1..]

signal :: Read a => String -> Signal a
signal = read . signal'

signal' :: String -> String
signal' [] = ""
signal' ('[': xs) = "Signal [" ++ signal' xs
signal' (c:xs)
    | isDigit c = let (s, e) = span isDigit xs in "Packet " ++ c : s ++ signal' e
    | otherwise = c : signal' xs


