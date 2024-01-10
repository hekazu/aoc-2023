{-# LANGUAGE TupleSections #-}

module Day21 (part1) where

import Data.List
import Data.Set (Set)
import qualified Data.Set as S

part1 :: IO ()
part1 = do
  rawInput <- lines <$> readFile "./input.txt"
  let extendedInput = map (\line -> "#" ++ line ++ "#") rawInput
  let linelength = length $ head extendedInput
  let input = [replicate linelength '#'] ++ extendedInput ++ [replicate linelength '#']
  let startPosition = findStart 0 input
  print . length . stepAround 64 [startPosition] $ readNoGo 0 input

findStart :: Int -> [String] -> (Int,Int)
findStart _ [] = error "Did not find start!"
findStart y (row:rows) = case 'S' `elemIndex` row of
  Just i -> (i,y)
  Nothing -> findStart (y+1) rows

readNoGo :: Int -> [String] -> Set (Int,Int)
readNoGo _ [] = S.empty
readNoGo y (row:rows) = S.union (S.fromList . map (,y) $ elemIndices '#' row) (readNoGo (y+1) rows)

stepAround :: Int -> [(Int,Int)] -> Set (Int,Int) -> [(Int,Int)]
stepAround 0 res _ = res
stepAround n from noGo = stepAround (n-1) (nub $ concatMap (\(x,y) -> filter (`S.notMember` noGo) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]) from) noGo
