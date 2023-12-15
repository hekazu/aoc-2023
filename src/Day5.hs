module Day5 where

import Data.Char (isDigit)

part1 :: IO ()
part1 = do
  input <- lines <$> readFile "./input.txt"
  let seeds = readSeeds $ head input
  let mappings = createMappings . dropWhile null $ dropWhile (not . null) input
  print . minimum $ transformStages seeds mappings

readSeeds :: String -> [Int]
readSeeds = map read . words . dropWhile (not . isDigit)

createMappings :: [String] -> [[((Int,Int),Int)]]
createMappings [] = []
createMappings input =
  map (createMapping . words) (drop 1 oneSet) : createMappings remaining
  where
    (oneSet,remaining) = split null [] input

createMapping :: [String] -> ((Int,Int),Int)
createMapping [sTo,sFrom,sRange] = ((from,from+range-1),to)
  where
    from = read sFrom
    to = read sTo
    range = read sRange
createMapping _ = error "Mangled input"

split :: Eq a => (a -> Bool) -> [a] -> [a] -> ([a],[a])
split _ acc [] = (acc,[])
split f acc (x:xs)
  | f x = (acc,xs)
  | otherwise = split f (acc ++ [x]) xs

transformStages :: [Int] -> [[((Int,Int),Int)]]  -> [Int]
transformStages =
  foldl (\acc stage -> map (transform stage) acc)

transform :: [((Int,Int),Int)] -> Int -> Int
transform [] value = value
transform (((from,to),dest):stage) value
  | value >= from && value <= to = value - from + dest
  | otherwise = transform stage value

part2 :: IO ()
part2 = do
  input <- lines <$> readFile "./input.txt"
  let seeds = readManySeeds $ head input
  let mappings = createMappings . dropWhile null $ dropWhile (not . null) input
  print . minimum $ transformStages seeds mappings

readManySeeds :: String -> [Int]
readManySeeds = rangify . readSeeds
  where
    rangify :: [Int] -> [Int]
    rangify [] = []
    rangify (from:range:rest) = [from..from+range-1] ++ rangify rest
    rangify _ = error "Odd number of seeds & ranges"
