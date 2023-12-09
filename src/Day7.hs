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
  -- If we only have one type of card (plus Jokers)
  | length groupedCards == 1 || numberOfJokers == 5 = FiveOfAKind cards
  -- If we have four of one type of card plus Jokers
  | any ((==4-numberOfJokers) . length) groupedCards = FourOfAKind cards
  -- And here is where the fun begins...
  --
  -- Should we have three cards of a type, or be able to reach that number
  -- with the help of Jokers...
  | any ((==3-numberOfJokers) . length) groupedCards =
      -- we will have used up our Jokers, as any more Jokers we would have
      -- used to get a Four of a Kind. So now we must have a spare pair
      -- for the Full House.
      --
      -- We have specifically two types of cards aside from Jokers, so one has
      -- to be a pair in case of Full House
      if length groupedCards == 2
        then FullHouse cards
        else ThreeOfAKind cards
  -- And if we have potential for a pair...
  -- We can only have one Joker, lest we immediately reach for a Three
  -- of a Kind. This is good to remember.
  | any ((==2-numberOfJokers) . length) groupedCards =
      -- And so, should we have
      --   3 kinds of cards, no Jokers -> There have to be two pairs
      --     since we do not have a Three of a Kind
      --   4 kinds and a Joker -> We have to have at least a pair, and
      --     then... dammit, that's another Three of a Kind. As such,
      --     it is impossible to have anything but the above for a Two Pair!
      if length groupedCards == 3
        then TwoPair cards
        else OnePair cards
  -- Out of that madness, this is all that is left.
  | otherwise = HighCard cards
  where
    cards = map (readCard Second) stringCards
    numberOfJokers = length $ filter (==Joker) cards
    cardsNoJokers = filter (/=Joker) cards
    groupedCards = group $ sort cardsNoJokers

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

part2 :: IO ()
part2 = do
  input <- lines <$> readFile "./input.txt"
  print . sum . calcWinnings . sort . map (readHandAndBid Second) $ input
