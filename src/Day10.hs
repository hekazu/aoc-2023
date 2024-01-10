module Day10 (part1) where

import Data.Graph
import Data.List
import Data.Maybe (catMaybes)

part1 :: IO ()
part1 = do
  input <- lines <$> readFile "./input.txt"
  let startPosition = findStart 0 input
  let graphListNoStart = allNodes 0 input
  let nodesConnectingToStart = filter (\(_,_,edgs) -> startPosition `elem` edgs) graphListNoStart
  let fullGraph = (startPosition, startPosition, map (\(label,_,_) -> label) nodesConnectingToStart) : graphListNoStart
  print . flip div 2 . length . head . filter (hasStart startPosition) . stronglyConnComp $ fullGraph

findStart :: Int -> [String] -> (Int,Int)
findStart _ [] = error "Did not find start"
findStart y (row:rows) = case 'S' `elemIndex` row of
  Just i -> (i,y)
  Nothing -> findStart (y+1) rows

hasStart :: (Int,Int) -> SCC (Int,Int) -> Bool
hasStart _startPos (AcyclicSCC _) = False
hasStart startPos (CyclicSCC vrts) = startPos `elem` vrts

allNodes :: Int -> [String] -> [((Int,Int),(Int,Int), [(Int,Int)])]
allNodes _ [] = []
allNodes y (row:rows) = catMaybes (rowNodes 0 y row) ++ allNodes (y+1) rows

rowNodes :: Int -> Int -> String -> [Maybe ((Int,Int),(Int,Int),[(Int,Int)])]
rowNodes _ _ "" = []
rowNodes x y (c:s) = nodify (x,y) c : rowNodes (x+1) y s

nodify :: (Int,Int) -> Char -> Maybe ((Int,Int), (Int,Int), [(Int,Int)])
nodify xy@(x,y) c
  | c == '|' = Just (xy,xy,[(x,y-1),(x,y+1)])
  | c == '-' = Just (xy,xy,[(x-1,y),(x+1,y)])
  | c == 'L' = Just (xy,xy,[(x,y-1),(x+1,y)])
  | c == 'J' = Just (xy,xy,[(x-1,y),(x,y-1)])
  | c == '7' = Just (xy,xy,[(x-1,y),(x,y+1)])
  | c == 'F' = Just (xy,xy,[(x,y+1),(x+1,y)])
  | otherwise = Nothing
