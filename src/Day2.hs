module Day2 where

import Data.List
import Data.Char (isAlpha)
import Data.Tuple (swap)
import qualified Data.Map as M

part1 :: IO ()
part1 = do
  input <- readFile "./input.txt"
  print . sumIds . filter isPossibleGame $ lines input

sumIds :: [String] -> Int
sumIds = sum . map (read . grabId)
  where
    grabId :: String -> String
    grabId = init . head . drop 1 . words

isPossibleGame :: String -> Bool
isPossibleGame = all possible . prepareGameInfo
  where
    possible (x,colour)
      | "red" `isPrefixOf` colour && x > 12 = False
      | "green" `isPrefixOf` colour && x > 13 = False
      | "blue" `isPrefixOf` colour && x > 14 = False
      | otherwise = True

pairs :: [String] -> [(Int,String)]
pairs [] = []
pairs (sx:colour:rest) = (read sx,colour) : pairs rest

part2 :: IO ()
part2 = do
  input <- readFile "./input.txt"
  print . sum . map gamePower $ lines input

gamePower :: String -> Int
gamePower =  product . M.elems . M.fromListWith max . map toAssocList . prepareGameInfo
  where
    toAssocList = (\(dirtyColour,x) -> (filter isAlpha dirtyColour,x)) . swap

prepareGameInfo :: String -> [(Int, String)]
prepareGameInfo = pairs . drop 2 . words
