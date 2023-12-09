module Day7 where

import Data.Bifunctor (bimap)
import Data.List (sort, group)

data Hand = FiveOfAKind [Card]
          | FourOfAKind [Card]
          | FullHouse [Card]
          | ThreeOfAKind [Card]
          | TwoPair [Card]
          | OnePair [Card]
          | HighCard [Card]
  deriving (Show,Eq,Ord)

-- When were you when you decided to rely on deriving Ord typeclass
-- ...and then couldn't be arsed to implement a custom comparator when
--    things got the slightest bit more difficult?
data Card = Ace
          | King
          | Queen
          | Jack
          | Ten
          | Nine
          | Eight
          | Seven
          | Six
          | Five
          | Four
          | Three
          | Two
          | Joker
  deriving (Show,Eq,Ord)

data Part = First | Second
  deriving Eq

part1 :: IO ()
part1 = do
  input <- lines <$> readFile "./input.txt"
  print . sum . calcWinnings . sort . map (readHandAndBid First) $ input

readHandAndBid :: Part -> String -> (Hand,Int)
readHandAndBid part =
  bimap
    (readHand part)
    (read . drop 1)
  . break (==' ')

readHand :: Part -> String -> Hand
readHand First stringCards
  | length (group cards) == 1 = FiveOfAKind cards
  | any ((==4) . length) groupedCards = FourOfAKind cards
  | any ((==3) . length) groupedCards = if any ((==2) . length) groupedCards
                                          then FullHouse cards
                                          else ThreeOfAKind cards
  | any ((==2) . length) groupedCards = if length groupedCards == 3
                                          then TwoPair cards
                                          else OnePair cards
  | otherwise = HighCard cards
  where
    cards = map (readCard First) stringCards
    groupedCards = group $ sort cards
readHand Second stringCards
  | length groupedCards == 1 = FiveOfAKind cards
  | any (inRange 4 numberOfJokers . length) groupedCards = FourOfAKind cards
  | any (inRange 3 numberOfJokers . length) groupedCards =
      if length groupedCards == 2
        then FullHouse cards
        else ThreeOfAKind cards
  | any (inRange 2 numberOfJokers . length) groupedCards =
      if length groupedCards == 3-numberOfJokers
        then TwoPair cards
        else OnePair cards
  | otherwise = HighCard cards
  where
    cards = map (readCard Second) stringCards
    numberOfJokers = length $ filter (==Joker) cards
    cardsNoJokers = filter (/=Joker) cards
    groupedCards = group $ sort cardsNoJokers

inRange :: Int -> Int -> Int -> Bool
inRange a b x = x == b-a || x == a

-- Not my proudest moment, but it'll do.
readCard :: Part -> Char -> Card
readCard part cardChar
  | cardChar == 'A' = Ace
  | cardChar == 'K' = King
  | cardChar == 'Q' = Queen
  | cardChar == 'J' = if part == First then Jack else Joker
  | cardChar == 'T' = Ten
  | cardChar == '9' = Nine
  | cardChar == '8' = Eight
  | cardChar == '7' = Seven
  | cardChar == '6' = Six
  | cardChar == '5' = Five
  | cardChar == '4' = Four
  | cardChar == '3' = Three
  | cardChar == '2' = Two
  | otherwise = error $ "Not a card: " ++ [cardChar]

calcWinnings :: [(Hand,Int)] -> [Int]
calcWinnings [] = []
calcWinnings ((_,bid):remaining) =
  bid * (length remaining + 1) : calcWinnings remaining

-- This currently gives a wrong answer, copypaste at your own responsibility
part2 :: IO ()
part2 = do
  input <- lines <$> readFile "./input.txt"
  print . sum . calcWinnings . sort . map (readHandAndBid Second) $ input
