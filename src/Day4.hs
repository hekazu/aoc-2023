module Day4 where

import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

part1 :: IO ()
part1 = do
  input <- readFile "./input.txt"
  print . sum . map (calculatePoints . findMatches . dropHeaders) $ lines input

-- I don't have any use for the card labels as they go in order anyway
dropHeaders :: String -> String
dropHeaders = drop 2 . dropWhile (/=':')

-- Given a row, break it into two separate lists of numbers and take their
-- set intersection
findMatches :: String -> [String]
findMatches = uncurry intersect . break (=="|") . words

-- Felt lazy, made an infinite list and looked up indices
calculatePoints :: [a] -> Int
calculatePoints = (pointsFor !!) . length
  where
    pointsFor = 0:unfoldr (\p -> Just (p,p*2)) 1


part2 :: IO ()
part2 = do
  -- opting to make lines early because their number is needed for extra setup
  input <- lines <$> readFile "./input.txt"
  let startingCardCount = M.fromAscList $ zip [1..length input] (repeat 1)
  print . foldr (+) 0 . countCards startingCardCount 1 . prepareData $ input
    where
      -- In short: As before, drop headers, find the matches and then
      -- turn all of the matched numbers into integers for nicer processing
      -- via IntMap instead of String matching
      prepareData = map (map read . findMatches . dropHeaders)

-- From an initial count of cards, figure out just how many of each card one
-- has in the end
countCards :: IntMap Int -> Int -> [[Int]] -> IntMap Int
countCards counts _ [] = counts
countCards counts card (matches:remaining) =
  countCards (winMore copiesOfCard wins counts) (card+1) remaining

  where
    wins = length matches
    copiesOfCard = counts M.! card

    -- The inputs are, in order:
    --   How many copies of the card that won we have
    --   How many wins that card had
    --   The running count of cards as an IntMap presentation
    winMore :: Int -> Int -> IntMap Int -> IntMap Int
    winMore _ 0 map = map
    winMore amount wins map =
      winMore amount (wins-1) $ M.adjust (+amount) (card+wins) map
