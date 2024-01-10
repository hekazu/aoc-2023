module Day18 (part1) where

part1 :: IO ()
part1 = do
  input <- map ((\[dir,l] -> (dir, read l)) . take 2 . words) . lines <$> readFile "./sampleinput.txt"
  print $ dig input

dig :: [(String,Int)] -> Bool
dig = undefined
