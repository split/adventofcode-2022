module Main where

import Control.Monad (foldM)
import Control.Monad.Trans.State (State, evalState, get, modify)
import Data.List (inits)
import Data.Map (Map)
import Data.Map qualified as Map

type FsMap = Map [String] Int

main = interact (unlines . sequence [part1, part2] . eval . lines)

part1 = ("Part 1: " ++) . show . sum . Map.filter (<= 100000)

part2 = ("Part 2: " ++) . maybe "Not found" show . cleanup

eval :: [String] -> FsMap
eval commands = evalState (foldM eval' Map.empty commands) []
  where
    eval' :: FsMap -> String -> State [String] FsMap
    eval' fs command = do
      cwd <- get
      case words command of
        ["$", "ls"] -> return fs
        ["dir", dir] -> return fs
        ["$", "cd", ".."] -> modify init >> return fs
        ["$", "cd", dir] -> modify (++ [dir]) >> return fs
        [size, name] -> return $ insertSize cwd (read size) fs

cleanup :: FsMap -> Maybe Int
cleanup fs = do
  used <- Map.lookup ["/"] fs
  return $ minimum $ filter (>= 30000000 - 70000000 + used) (Map.elems fs)

insertSize :: [String] -> Int -> FsMap -> FsMap
insertSize path size fs = foldr (\k -> Map.insertWith (+) k size) fs (inits path)