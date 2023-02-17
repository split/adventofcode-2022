module Main where

import Control.Applicative ((<|>))
import Control.Monad (ap, foldM, guard, (<$!>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (runStateT), evalState, get, modify)
import Data.Bifunctor (Bifunctor (first))
import Data.Heap qualified as Map
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Debug.Trace (trace)

-- data Robot =

-- data Blueprint = Blueprint { robots :: Robots } deriving (Show)

-- data State = State { production :: Resources, resources :: Resources } deriving (Show, Eq)

type Resources = Map String (Int, Int)

type Blueprint = Map String [(String, Int)]

-- data Unit = Unit String Int deriving (Eq, Ord)

-- instance Num Unit where

-- runTurn :: Blueprint -> Resources -> Resources -> Int -> State (Int, Resources) Resources
-- runTurn bp = do
--   (turn, resources) <- get

-- return $ fromMaybe () ()

-- instance Show Unit where
--   show (Unit unit amount) = show amount ++ " " ++ unit

-- turn :: Blueprint -> Int -> Int
turn bp time = runStateT (memgo [start] 0) M.empty
  where
    start = (M.singleton "ore" (0, 1), time)

    -- memgo :: [(Resources, Int)] -> StateT (Map Resources Int) Int
    -- All has been processed, pick the best
    memgo [] best =
      trace ("Best!!: " ++ show best) $
        return best
    -- Time has ended. Let's count our geodes
    memgo ((res, 0) : xs) best = do
      (amount, prod) <- lift $ res M.!? "geode"
      memgo xs (max (amount + prod) best)

    -- One more path to test for
    memgo ((res, tl) : xs) best = do
      mem <- get
      case mem M.!? res of
        Just tl' | tl' >= tl -> memgo xs best
        _ -> modify (M.insert res tl) >> memgo (xs ++ compute res tl) best

    compute :: Resources -> Int -> [(Resources, Int)]
    compute resources tl =
      let r = map ((,tl - 1) . mine) (resources : bought)
       in -- trace (show tl ++ ": Computing with " ++ show resources ++ " with resulting " ++ show r)
          r
      where
        bought = buyPermultations bp resources
        mine = M.map (\(amount, prod) -> (amount + prod, prod))

-- bought = M.foldrWithKey buy [resources] bp
-- buy unit costs rs = mapMaybe buy' rs : rs
--   where
--     buy' res' = do
--       let usedResources = map (\(unit, a) -> (unit, a - amount unit res')) costs
--       guard (all (>= 0) usedResources)

-- Time end, return the result
-- go ((res, 0): xs) = do
--   (amount, prod) <- res M.!? "geode"
--   return (amount + prod)

-- compute resources 0 = do
--     tl' <- pure (mem M.!? res)

--     return Nothing

-- nothing <- go ((mined, tl - 1) : xs)

amount :: String -> Resources -> Int
amount = (maybe 0 fst .) . M.lookup

buyPermultations :: Blueprint -> Resources -> [Resources]
-- M.foldrWithKey (((:) .) . buyRobot) [] bp
buyPermultations bp resources =
  -- trace ("Robot needs: " ++ show robotNeeds) $
  let buy = catMaybes $ M.foldrWithKey (((:) .) . buyRobot) [] bp in trace ("RES: " ++ show resources ++ "\nBUY: " ++ show buy) buy
  where
    buyRobot :: String -> [(String, Int)] -> Maybe Resources
    buyRobot unit costs = do
      need <- robotNeeds M.!? unit <|> return 0
      (amount, robots) <- resources M.!? unit <|> return (0, 0)
      guard (amount <= need && robots <= need)
      M.insertWith add unit (0, 1) <$> updateAmounts unit costs

    updateAmounts unit = foldM updateAmount resources
    updateAmount res' (cost, amount) = do
      newAmount <- (+ (-amount)) . fst <$> cost `M.lookup` res'
      guard (newAmount >= 0)
      return $ M.adjust (first (const newAmount)) cost res'

    robotNeeds = let r = M.insert "geode" 9999 $ M.fromListWith (+) $ concat $ M.elems bp in r

main = interact (unlines . sequence [part1] . parse)

part1 = ("Part 1: " ++) . show -- . map (`turn` 30)

parse :: String -> [Blueprint]
parse = map (M.fromList . map parseRobot . splitOn ". " . dropWhile (/= 'E')) . lines
  where
    parseRobot = ap ((,) . head) (parseCosts . drop 2) . tail . words
    parseCosts (_ : amount : unit : xs) = (filter (/= '.') unit, read amount) : parseCosts xs
    parseCosts _ = []

add (a, b) (c, d) = (a + c, b + d)