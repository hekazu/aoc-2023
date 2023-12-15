{-# LANGUAGE TupleSections #-}

module Day15 where

import Data.Char
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.List

part1 :: IO ()
part1 = do
  input <- readFile "./input.txt"
  print . sum . map (runHASH 0) $ splitAtComma input

splitAtComma :: String -> [String]
splitAtComma [] = []
splitAtComma s = next : splitAtComma (drop (length next + 1) s)
  where
    next = takeWhile (/=',') $ dropWhile (==',') s

runHASH :: Int -> String -> Int
runHASH val [] = val
runHASH val (c:s)
  | c == '\n' = runHASH val s
  | otherwise = runHASH newHash s
  where
    newHash = (`mod` 256) . (*17) $ ord c + val

part2 :: IO ()
part2 = do
  input <- readFile "./input.txt"
  print . M.foldrWithKey calculateFocusPower 0 . foldl' processOperation emptyBoxes $ splitAtComma input

data Lens = Lens {lensLabel :: String, focalLength :: Int}
  deriving Show

-- This is hacky and you should never do this outside of a controlled
-- puzzle solving environment
instance Eq Lens where
  (==) = (==) `on` lensLabel

emptyBoxes :: IntMap [Lens]
emptyBoxes = M.fromList (map (,[]) [0..255])

processOperation :: IntMap [Lens] -> String -> IntMap [Lens]
processOperation boxes commandString
  | head command == '-' = M.adjust (delete (Lens label 0)) (boxHASH label) boxes
  | head command == '=' = M.adjust (handleBox (Lens label (read $ drop 1 command))) (boxHASH label) boxes
  | otherwise = boxes
  where
    (label,command) = break (`elem` "=-") commandString

handleBox :: Lens -> [Lens] -> [Lens]
handleBox lens box
  | lens `elem` box = replaceLens box
  | otherwise = lens : box
  where
    replaceLens [] = error "Expected lens not in lens box"
    replaceLens (oldLens:lenses)
      | lens == oldLens = lens : lenses
      | otherwise = oldLens : replaceLens lenses

boxHASH :: String -> Int
boxHASH = runHASH 0 . takeWhile isAlpha

calculateFocusPower :: M.Key -> [Lens] -> Int -> Int
calculateFocusPower _         []            fPower = fPower
calculateFocusPower boxNumber slots@(lens:lenses) fPower =
  calculateFocusPower boxNumber lenses incPower
  where
    incPower = fPower + (boxNumber+1) * length slots * focalLength lens

hasLabel :: Lens -> String -> Bool
hasLabel lens label = lensLabel lens == label
