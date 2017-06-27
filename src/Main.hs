module Main where

import Data.List (concatMap)

data Suit = Bamboo | Character | Coin
  deriving (Eq, Show)
data Rank = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq, Show)
data Card = Flower | Suited Suit Rank | Dragon Suit
  deriving (Eq, Show)
type Deck = [Card]

type FreeCell = Maybe Card
type Stack = [Card]
type Column = [Card]
data Layout = Layout [FreeCell] [Stack] [Column]
  deriving (Show)

type MoveCount = Integer
data Game = Game Layout MoveCount

deck :: Deck
deck = Flower : concatMap suitcards suits
  where
    suits = [Bamboo, Character, Coin]
    suitcards suit = replicate 4 (Dragon suit) ++ map (Suited suit)
                     [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

main :: IO ()
main = do
  putStrLn "hello world"
