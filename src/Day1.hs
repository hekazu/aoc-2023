module Day1 where

import Data.Char (isDigit)

part1 :: IO ()
part1 = do
  input <- readFile "./input.txt"
  print . sum . map (readProperDigits . filter isDigit) $ lines input

readProperDigits :: String -> Int
readProperDigits [x] = read $ x : [x]
readProperDigits (x:xs) = read $ x : [last xs]

part2 :: IO ()
part2 = do
  input <- readFile "./input.txt"
  print . sum . map convertLine $ lines input

convertLine :: String -> Int
convertLine = readProperDigits . filter isDigit . digitize

-- Thanks I Hate It
-- Yet, annoyingly, the patterns could arbitrarily overlap so doing plain
-- replaces wouldn't do it. At least after finding a digit I could proceed from
-- the very next letter without yeeting the rest given it could not change the
-- order of appearance at that stage
digitize :: String -> String
digitize [] = []
digitize (c:s)
    | c == 'o' = if take 2 s == "ne" then '1':next else c:next
    | c == 't' = if take 2 s == "wo" then '2':next
                   else if take 4 s == "hree" then '3':next else c:next
    | c == 'f' = if take 3 s == "our" then '4':next
                   else if take 3 s == "ive" then '5':next else c:next
    | c == 's' = if take 2 s == "ix" then '6':next
                   else if take 4 s == "even" then '7':next else c:next
    | c == 'e' = if take 4 s == "ight" then '8':next else c:next
    | c == 'n' = if take 3 s == "ine" then '9':next else c:next
    | otherwise = c:next
    where next = digitize s
