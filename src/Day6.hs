module Day6 where

import Data.List
import Data.Char (isDigit)

part1 :: IO ()
part1 = do
  times <- fmap (map read . words . dropWhile (not . isDigit)) getLine
  goals <- fmap (map read . words . dropWhile (not . isDigit)) getLine
  print . product . map length $ findWinningWaits times goals

findWinningWaits :: [Int] -> [Int] -> [[Int]]
findWinningWaits (time:times) (goal:goals) =
  filter (>goal) (unfoldr (makeRun time) 1) : findWinningWaits times goals
findWinningWaits _ _ = []

makeRun :: Int -> Int -> Maybe (Int,Int)
makeRun time wait
  | time == wait = Nothing
  | otherwise = Just (wait*(time-wait), wait+1)

part2 :: IO ()
part2 = do
  time <- read . filter isDigit <$> getLine
  goal <- read . filter isDigit <$> getLine
  print . length . concat $ findWinningWaits [time] [goal]
