module Day14 where

import Data.List
import Data.Ord (Down(..), comparing)

part1 :: IO ()
part1 = do
  input <- lines <$> readFile "./input.txt"
  let northshifted = map rollStones $ transpose input
  print . sum $ map calculateRowLoad northshifted

rollStones :: String -> String
rollStones [] = []
rollStones objects =
  sortBy (comparing Down) rollers ++ stationaries ++ rollStones rest
  where
    rollers = takeWhile (/='#') objects
    stationaries = takeWhile (=='#') $ dropWhile (/='#') objects
    rest = drop (length rollers + length stationaries) objects

calculateRowLoad :: String -> Int
calculateRowLoad [] = 0
calculateRowLoad full@(object:row)
  | object == 'O' = length full + calculateRowLoad row
  | otherwise = calculateRowLoad row
