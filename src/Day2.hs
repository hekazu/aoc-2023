module Day2 where

import Data.List
import Data.Bifunctor (first)
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

-- Check whether a game is possible with the given contraints on draws
isPossibleGame :: String -> Bool
isPossibleGame = all possible . prepareGameInfo
  where
    possible (x,colour)
      | "red" `isPrefixOf` colour && x > 12 = False
      | "green" `isPrefixOf` colour && x > 13 = False
      | "blue" `isPrefixOf` colour && x > 14 = False
      | otherwise = True

part2 :: IO ()
part2 = do
  input <- readFile "./input.txt"
  print . sum . map gamePower $ lines input

-- Calculate the "power" of a single game
gamePower :: String -> Int
gamePower =  product . M.elems . M.fromListWith max . map toAssocList . prepareGameInfo
  where
    -- Turn colours into association list keys, stripping trailing commas and
    -- semicolons
    toAssocList = first (filter isAlpha) . swap

-- Take the information about a game and turn it into a list of pairs
-- representing draws, ordered like in source code
prepareGameInfo :: String -> [(Int, String)]
prepareGameInfo = pairs . drop 2 . words
  where
    pairs :: [String] -> [(Int,String)]
    pairs [] = []
    pairs (sx:colour:rest) = (read sx,colour) : pairs rest
