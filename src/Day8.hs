module Day8 where

import Data.Map (Map)
import qualified Data.Map as M

part1 :: IO ()
part1 = do
  input <- lines <$> readFile "./input.txt"
  let directions = cycle $ head input
  let nodes = M.fromList . map (toAssocElement . words) $ drop 2 input
  print $ travel 0 "AAA" nodes directions

toAssocElement :: [String] -> (String,(String,String))
toAssocElement [key,_,left,right] = (key,(init $ drop 1 left, init right))

travel :: Int -> String -> Map String (String, String) -> String -> Int
travel steps "ZZZ" _ _ = steps
travel steps from stepMap (whereTo:instructions)
  | whereTo == 'L' = travel (steps+1) (stepL stepMap from) stepMap instructions
  | whereTo == 'R' = travel (steps+1) (stepR stepMap from) stepMap instructions
  | otherwise = error "Unidentified direction"

stepL :: Map String (String,String) -> String -> String
stepL stepMap from = fst $ stepMap M.! from

stepR :: Map String (String,String) -> String -> String
stepR stepMap from = snd $ stepMap M.! from

-- part2 :: IO ()
-- part2 = do
--   input <- lines <$> readFile "./input.txt"
--   let directions = cycle $ head input
--   let nodes = M.fromList . map (toAssocElement . words) $ drop 2 input
--   print $ ghostTravel 0 (filter ((=='A') . last) $ M.keys nodes) nodes directions
-- 
-- ghostTravel :: Int -> [String] -> Map String (String, String) -> String -> Int
-- ghostTravel steps departures stepMap (whereTo:instructions)
--   | all ((=='Z') . last) departures = steps
--   | whereTo == 'L' =
--     ghostTravel (steps+1) (map (stepL stepMap) departures) stepMap instructions
--   | whereTo == 'R' =
--     ghostTravel (steps+1) (map (stepR stepMap) departures) stepMap instructions
--   | otherwise = error "Unidentified direction"
