{-# LANGUAGE TupleSections #-}

module Day11 where

import Data.Function (on)
import Data.List

part1 :: IO ()
part1 = do
  rawInput <- lines <$> readFile "./input.txt"
  let input = expand . transpose $ expand rawInput
  print . sum . concat . manhattans . concat . addYCoords 0 $ map (findGalaxies 0) input

expand :: [String] -> [String]
expand [] = []
expand (line:s)
  | all (=='.') line = line : line : expand s
  | otherwise = line : expand s

findGalaxies :: Int -> String -> [Int]
findGalaxies _ [] = []
findGalaxies i (c:s)
  | c == '#' = i : next
  | c == '+' = findGalaxies (i+1000000) s
  | otherwise = next
  where
    next = findGalaxies (i+1) s

addYCoords :: Int -> [[Int]] -> [[(Int,Int)]]
addYCoords _ [] = []
addYCoords y (xs:xss) = map (,y) xs : addYCoords (y+1) xss

manhattans :: [(Int,Int)] -> [[Int]]
manhattans [] = []
manhattans (current:coords) = map (manhattan current) coords : manhattans coords
  where
    manhattan (x1,y1) (x2,y2) = ((+) `on` abs) (x1-x2) (y1-y2)

part2 :: IO ()
part2 = do
  rawInput <- lines <$> readFile "./input.txt"
  let input = hugeExpand . transpose $ hugeExpand rawInput
  print . sum . concat . manhattans $ find2dGalaxies 0 input

hugeExpand :: [String] -> [String]
hugeExpand [] = []
hugeExpand (line:s)
  | all (`elem` ".+") line = replicate (length line) '+' : hugeExpand s
  | otherwise = line : hugeExpand s

find2dGalaxies :: Int -> [String] -> [(Int,Int)]
find2dGalaxies _ [] = []
find2dGalaxies y (line:space)
  | all (=='+') line = find2dGalaxies (y+1000000) space
  | otherwise = map (,y) (findGalaxies 0 line) ++ find2dGalaxies (y+1) space
