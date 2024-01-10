module Day13 (part1) where

import Data.List

part1 :: IO ()
part1 = do
  input <- matrify . lines <$> readFile "./input.txt"
  print . sum $ map pointify input

matrify :: [String] -> [[String]]
matrify [] = []
matrify s = takeWhile (not . null) s : matrify (drop 1 $ dropWhile (not . null) s)

pointify :: [String] -> Int
pointify matrix = case pointifyX 1 matrix of
  Just index -> index
  Nothing -> maybe 0 (100*) pointifyY
  where
    pointifyY = pointifyX 1 $ transpose matrix

pointifyX :: Int -> [String] -> Maybe Int
pointifyX ix matrix
  | ix == length (head matrix) = Nothing
  | all (\row -> reflects (take ix row) (drop ix row)) matrix = Just ix
  | otherwise = pointifyX (ix+1) matrix

reflects :: String -> String -> Bool
reflects s1 s2
  | s1 == "" || s2 == "" = True
  | otherwise = last s1 == head s2 && reflects (init s1) (tail s2)
