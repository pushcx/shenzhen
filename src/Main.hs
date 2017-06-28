module Main where

import Data.List (concatMap, genericIndex, intercalate, transpose)
import Data.List.Split (chunksOf)
import System.Random.Shuffle (shuffleM)

data Suit = Bamboo | Character | Dot
  deriving (Eq)

suits = [Bamboo, Character, Dot]

instance Show Suit where
  show Bamboo    = "b"
  show Character = "c"
  show Dot       = "d"


data Rank = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq)

instance Show Rank where
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"


data Card = Flower | Suited Rank Suit | Dragon Suit

instance Show Card where
  show Flower             = "Fl"
  show (Dragon suit)      = "D" ++ show suit
  show (Suited rank suit) = show rank ++ show suit
type Deck = [Card]

suitOf :: Card -> Suit
suitOf (Suited _ s) = s
suitOf (Dragon s) = s
suitOf _ = error "can't get suit of Flower"

rankOf :: Card -> Rank
rankOf (Suited r _) = r
rankOf _ = error "can't get rank of Flower/Dragon"


newtype FreeCell = FreeCell (Maybe Card)
type FlowerCell = FreeCell

instance Show FreeCell where
  show (FreeCell Nothing)  = "__"
  show (FreeCell (Just c)) = show c


data Stack = Stack Suit [Card]

instance Show Stack where
  show (Stack suit []) = "_" ++ show suit
  show (Stack suit cs) = show . head $ reverse cs
type Column = [Card]

showcols :: [Column] -> String
showcols cs = intercalate "\n" $ map (intercalate "  " . map show) (transpose cs)


data Layout = Layout [FreeCell] FlowerCell [Stack] [Column]
instance Show Layout where
  show (Layout fcs f ss cs) =
       "F: " ++ unwords (map show fcs)
    ++ " Fl: "  ++ show f
    ++ " -> "   ++ unwords (map show ss)
    ++ "\n"     ++ showcols cs

type MoveCount = Integer
data Game = Game Layout MoveCount
  deriving (Show)

type ColumnIndex = Integer
type FreeIndex = Integer

nextRank :: Rank -> Maybe Rank
nextRank One   = Just Two
nextRank Two   = Just Three
nextRank Three = Just Four
nextRank Four  = Just Five
nextRank Six   = Just Seven
nextRank Seven = Just Eight
nextRank Eight = Just Nine
nextRank Nine  = Nothing


nextCardForStack :: Stack -> Maybe Card
nextCardForStack (Stack suit (Suited r s:_)) = fmap (flip Suited suit) (nextRank r)
nextCardForStack (Stack suit []) = Just (Suited One suit)

stackBySuit :: Suit -> [Stack] -> Stack
stackBySuit suit stacks = head $ filter (\(Stack s _) -> s == suit) stacks

data Move =
    MoveSingleFromColumnToFree ColumnIndex FreeIndex
  | MoveSingleFromFreeToColumn FreeIndex ColumnIndex
  | FinishColumn ColumnIndex
  | FinishFree FreeIndex
  | MoveRun ColumnIndex Card ColumnIndex
--mkFinishFree :: Game -> FreeIndex -> Maybe Move
--mkFinishFree (Game (Layout fcs _ stacks _) _) i
--  | i < (length fcs - 1) && nextCardForStack (stackBySuit suit stacks) == card = Just (FinishFree i)
--  | otherwise = Nothing
--  where
--    card = genericIndex fcs i
--    suit = suitOf card
--    rank = rankOf card

deck :: Deck
deck = Flower : concatMap suitcards suits
  where
    suitcards suit = replicate 4 (Dragon suit) ++ map (\r -> Suited r suit)
                     [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

layout :: Deck -> Layout
layout deck = Layout
             [FreeCell Nothing, FreeCell Nothing, FreeCell Nothing]
             (FreeCell Nothing) -- TODO FlowerCell?
             (map (\suit -> Stack suit []) suits)
             (chunksOf 5 deck)

move :: Game -> Move -> Game
move g m = undefined

main :: IO ()
main = do
  shuffled <- shuffleM deck
  print (layout shuffled)
