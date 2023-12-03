module Day3 where

import Data.Bifunctor
import Data.Char (isDigit)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

-- Named enum used to determine whether looking for any and all symbols, or just
-- potential gears
data SymbolSelection = Any | MaybeGear
  deriving Eq

part1 :: IO ()
part1 = do
  input <- readFile "./input.txt"
  print . sum . uncurry numbersInSchematic . trawlSchematicLines Any 0 $ lines input

-- Handle looking for both part numbers and symbols of sought after kind from input
trawlSchematicLines
  :: SymbolSelection
  -> Int
  -> [String]
  -> (Map [(Int,Int)] Int, [(Int,Int)])
trawlSchematicLines _ _ [] = (M.empty, [])
trawlSchematicLines select y (l:ls) =
  bimap
    (M.union numbers)
    (++ symbols)
    (trawlSchematicLines select (y+1) ls)
  where
    (numbers,symbols) = trawlSchematicLine select (0,y) l

-- Take care of a single input line
trawlSchematicLine
  :: SymbolSelection
  -> (Int,Int)
  -> String
  -> (Map [(Int,Int)] Int, [(Int,Int)])
trawlSchematicLine _ _ [] = (M.empty,[])
trawlSchematicLine select (x,y) (c:s)
  | isDigit c = first (M.insert calculateKeyValues (read fullNumberString)) skipFewAndGo
  | select == Any && c `S.notMember` nonsymbols = second ((x,y):) go
  | select == MaybeGear && c == '*' = second ((x,y):) go
  | otherwise = go
  where
    fullNumberString = c:takeWhile isDigit s
    calculateKeyValues = [(i,y) | i <- [x..x+length fullNumberString-1]]
    skipFewAndGo =
      trawlSchematicLine select (x+length fullNumberString,y) (dropWhile isDigit s)
    go = trawlSchematicLine select (x+1,y) s

nonsymbols :: Set Char
nonsymbols = S.fromList $ '.' : ['1'..'9']

-- Determine which numbers are actually part of the schematic and not just decoration
numbersInSchematic :: Map [(Int,Int)] Int -> [(Int,Int)] -> [Int]
numbersInSchematic _ [] = []
numbersInSchematic partNumberMap ((x,y):symbols) =
  findPartsAttachedToSymbol ++ numbersInSchematic partNumberMap symbols
  where
    findPartsAttachedToSymbol = foldl (\acc k -> (partNumberMap M.! k) : acc) [] findKeysForSymbolAdjacent
    findKeysForSymbolAdjacent = filter (not . null . intersect symbolSpaces) (M.keys partNumberMap)
    symbolSpaces = [(x1,y1) | x1 <- [x-1..x+1], y1 <- [y-1..y+1]]

part2 :: IO ()
part2 = do
  input <- readFile "./input.txt"
  print . sum . uncurry gearRatiosInSchematic . trawlSchematicLines MaybeGear 0 $ lines input

-- Calculate the gear ratios in the schematic, discarding invalid gears
gearRatiosInSchematic :: Map [(Int,Int)] Int -> [(Int,Int)] -> [Int]
gearRatiosInSchematic _ [] = []
gearRatiosInSchematic partNumberMap ((x,y):symbols)
  | null findGearRatios = gearRatiosInSchematic partNumberMap symbols
  | otherwise = product findGearRatios : gearRatiosInSchematic partNumberMap symbols
  where
    findGearRatios
      | length findGearKeys == 2 =
        foldl (\acc k -> (partNumberMap M.! k) : acc) [] findGearKeys
      | otherwise = []
    findGearKeys = filter (not . null . intersect symbolSpaces) (M.keys partNumberMap)
    symbolSpaces = [(x1,y1) | x1 <- [x-1..x+1], y1 <- [y-1..y+1]]
