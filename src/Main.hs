module Main where

import Data.List (concatMap, intercalate, transpose)
import Data.List.Split (chunksOf)
import System.Random.Shuffle (shuffleM)

data Suit = Bamboo | Character | Dot
  deriving (Eq)
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

newtype FreeCell = FreeCell (Maybe Card)
instance Show FreeCell where
  show (FreeCell Nothing)  = "__"
  show (FreeCell (Just c)) = show c
type FlowerCell = FreeCell
newtype Stack = Stack [Card]
instance Show Stack where
  show (Stack []) = "__"
  show (Stack cs) = show . head $ reverse cs
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

deck :: Deck
deck = Flower : concatMap suitcards suits
  where
    suits = [Bamboo, Character, Dot]
    suitcards suit = replicate 4 (Dragon suit) ++ map (\r -> Suited r suit)
                     [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

start :: Deck -> Layout
start deck = Layout
             [FreeCell Nothing, FreeCell Nothing, FreeCell Nothing]
             (FreeCell Nothing)
             [Stack [], Stack [], Stack []]
             (chunksOf 5 deck)

main :: IO ()
main = do
  shuffled <- shuffleM deck
  print (start shuffled)
