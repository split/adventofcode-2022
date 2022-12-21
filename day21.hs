module Main where

import Control.Monad (ap)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M

type Monkeys a = Map String (Job a)

data Job a = YellNumber a | BinaryWork String Char String deriving (Show)

main = interact (unlines . sequence [part1] . parse)

part1 = ("Part 1: " ++) . show . work "root"

work :: String -> Monkeys Int -> Int
work monkey monkeys = runWork monkey
  where
    runWork = work' . (monkeys M.!)
    work' (YellNumber n) = n
    work' (BinaryWork m1 '+' m2) = runWork m1 + runWork m2
    work' (BinaryWork m1 '-' m2) = runWork m1 - runWork m2
    work' (BinaryWork m1 '*' m2) = runWork m1 * runWork m2
    work' (BinaryWork m1 '/' m2) = runWork m1 `div` runWork m2
    work' (BinaryWork m1 op m2) = error ("Invalid operation " ++ [op])

parse :: String -> Monkeys Int
parse = M.fromList . map (ap ((,) . init . head) (parseJob . tail) . words) . lines

parseJob :: [String] -> Job Int
parseJob (a : op : b : _) = BinaryWork a (head op) b
parseJob [n] = YellNumber (read n)
parseJob x = error ("Invalid job: " ++ show x)