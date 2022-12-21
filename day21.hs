module Main where

import Control.Monad (ap)
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M

type Monkeys a = Map String (Job a)

data Job a = YellNumber a | TellNumber | BinaryWork String Char String deriving (Show)

main = interact (unlines . sequence [part1, part2] . parse)

part1 = ("Part 1: " ++) . show . work "root"

part2 = ("Part 2: " ++) . show . work "root" . patchMonkeys

-- work :: String -> Monkeys Int -> Either (Int -> Int) Int
work :: String -> Monkeys Int -> Int
work monkey monkeys = either ($ 0) id $ runWork monkey
  where
    runWork :: String -> Either (Int -> Int) Int
    runWork = work' . (monkeys M.!)

    work' :: Job Int -> Either (Int -> Int) Int
    work' (YellNumber n) = return n
    work' TellNumber = Left id
    work' (BinaryWork m1 opc m2) = do
      let (op, revopL, revopR) = operation opc
      let w1 = runWork m1
      let w2 = runWork m2
      a <- applyRev w1 revopL w2
      b <- applyRev w2 revopR w1
      return (a `op` b)

applyRev (Left revop') revop job = do
  value <- job
  Left (revop' . flip revop value)
applyRev job _ _ = job

-- Operations with opposite ones
operation '+' = ((+), (-), (-)) -- a + b = c -> a = c - b, b = c - a
operation '-' = ((-), (+), flip (-)) -- a - b = c -> a = c + b, b = a - c
operation '*' = ((*), div, div) -- a * b = c -> a = c / b, b = c / b
operation '/' = (div, (*), flip div) -- a / b = c -> a = c * b, b = a / c
operation '=' = (const, const id, const id)

patchMonkeys =
  M.adjust (\(BinaryWork a _ b) -> BinaryWork a '=' b) "root"
    . M.insert "humn" TellNumber

parse :: String -> Monkeys Int
parse = M.fromList . map (ap ((,) . init . head) (parseJob . tail) . words) . lines

parseJob :: [String] -> Job Int
parseJob (a : op : b : _) = BinaryWork a (head op) b
parseJob [n] = YellNumber (read n)
parseJob x = error ("Invalid job: " ++ show x)