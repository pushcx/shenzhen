module Main where

data Suit = Bamboo | Character | Coin
  deriving (Eq, Show)
data Rank = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq, Show)
data Card = Flower | Suited Suit Rank | Dragon Suit
  deriving (Eq, Show)
type Deck = [Card]

main :: IO ()
main = do
  putStrLn "hello world"
