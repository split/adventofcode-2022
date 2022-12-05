import Control.Monad (ap)
import Data.Char (isAlpha, isDigit)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

type Stacks = Map Char String

type Move = (Int, Char, Char)

main = interact (unlines . sequence [part1, part2] . parse)

part1 (moves, stacks) = "Part 1: " ++ mapMaybe listToMaybe (Map.elems $ foldl (move reverse) stacks moves)

part2 (moves, stacks) = "Part 2: " ++ mapMaybe listToMaybe (Map.elems $ foldl (move id) stacks moves)

move :: (String -> String) -> Stacks -> Move -> Stacks
move lift stacks (n, from, to) = fromMaybe stacks result
  where
    result = do
      moved <- lift . take n <$> Map.lookup from stacks
      return $ Map.adjust (moved ++) to (Map.adjust (drop n) from stacks)

parse = ap ((,) . parseMoves) parseStacks . splitOn "\n\n"

parseStacks :: [String] -> Stacks
parseStacks = Map.fromList . map (\l -> (last l, filter isAlpha l)) . filter (isDigit . last) . transpose . lines . head

parseMoves :: [String] -> [Move]
parseMoves = map parseMove . lines . (!! 1)

parseMove :: String -> Move
parseMove = (\(k : f : t : _) -> (read k, head f, head t)) . filter (isDigit . head) . words