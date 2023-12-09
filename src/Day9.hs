module Day9 where

part1 :: IO ()
part1 = do
  input <- lines <$> readFile "./input.txt"
  print . sum $ map (sum . map last . extrapolate . map read . words) input

extrapolate :: [Int] -> [[Int]]
extrapolate lineNums
  | all (==0) lineNums = [lineNums]
  | otherwise = lineNums : extrapolate diffs
  where
    diffs = calcDiffs lineNums

calcDiffs :: [Int] -> [Int]
calcDiffs [_] = []
calcDiffs (x:y:nums) = y-x:calcDiffs (y:nums)
calcDiffs e = error $ "Input mangled at " ++ show e

part2 :: IO ()
part2 = do
  input <- lines <$> readFile "./input.txt"
  print . sum $ map fun input
  where
    -- As we can see, if the numbers are the other way round we get
    -- results in the past 0:)
    fun = sum . map last . extrapolate . reverse . map read . words
