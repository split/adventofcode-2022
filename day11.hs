{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (ap, (<=<))
import Control.Monad.Trans.State (State, modify)
import Data.List (singleton, sortOn)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (Down))
import Debug.Trace (trace)

data Monkey = Monkey
  { mid :: String,
    items :: [Int],
    op :: Int -> Int,
    test :: Int -> String,
    inspected :: Int
  }

type MonkeyReg = Map String Monkey

instance Show Monkey where
  show m = "Monkey " ++ mid m ++ ": " ++ show (items m)

main = interact (unlines . sequence [part1] . toReq . parse)

part1 = ("Part 1: " ++) . show . monkeyBusiness . (!! 20) . iterate inspectRound

monkeyBusiness = product . take 2 . sortOn Down . map inspected . Map.elems

inspectRound :: MonkeyReg -> MonkeyReg
inspectRound reg = foldl (\reg' k -> inspect (reg' Map.! k) reg') reg (Map.keys reg)

inspect :: Monkey -> MonkeyReg -> MonkeyReg
inspect monkey@Monkey {..} = markInspected . throwInspected
  where
    markInspected = Map.insert mid (monkey {items = [], inspected = inspected + length items})
    throwInspected = flip (foldr (uncurry throw)) (map (inspectItem monkey) items)

inspectItem :: Monkey -> Int -> (Int, String)
inspectItem Monkey {..} = ap (,) test . (`div` 3) . op

throw :: Int -> String -> MonkeyReg -> MonkeyReg
throw = Map.adjust . alterItems . (++) . singleton

parse :: [Char] -> [Monkey]
parse = map (monkey' . lines) . splitOn "\n\n"
  where
    monkey' (mid : items : op : test : t : f : _) = Monkey mid' items' op' test' 0
      where
        mid' = init $ last $ words mid
        items' = map read . splitOn ", " $ last $ splitOn ": " items
        op' = execOp $ drop 3 $ words op
        div' = read $ val' test
        test' old = val' $ if old `mod` div' == 0 then t else f
        val' = last . words

toReq :: [Monkey] -> MonkeyReg
toReq = Map.fromList . map (ap ((,) . mid) id)

alterItems :: ([Int] -> [Int]) -> Monkey -> Monkey
alterItems f monkey = monkey {items = f (items monkey)}

execOp (l : "+" : r : _) = \old -> val l old + val r old
execOp (l : "*" : r : _) = \old -> val l old * val r old
execOp x = error ("Invalid operation: " ++ show x)

val :: String -> Int -> Int
val "old" = id
val num = const (read num)