module Main where

import Data.List (concatMap, genericIndex, intercalate, transpose)
import Data.List.Split (chunksOf)
import Safe (headMay)
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


data Card = Flower | Suited Suit Rank | Dragon Suit
  deriving (Eq)

instance Show Card where
  show Flower             = "Fl"
  show (Dragon suit)      = "D" ++ show suit
  show (Suited suit rank) = show rank ++ show suit
type Deck = [Card]

suitOf :: Card -> Suit
suitOf (Suited s _) = s
suitOf (Dragon s) = s
suitOf _ = error "can't get suit of Flower"

rankOf :: Card -> Rank
rankOf (Suited _ r) = r
rankOf _ = error "can't get rank of Flower/Dragon"


newtype Cell = Cell (Maybe Card)
type FlowerCell = Cell

instance Show Cell where
  show (Cell Nothing)  = "__"
  show (Cell (Just c)) = show c


data Foundation = Foundation Suit [Card]

instance Show Foundation where
  show (Foundation suit []) = "_" ++ show suit
  show (Foundation suit cs) = show . head $ reverse cs


-- A vertical stack of cards. Stored topmost-first.
-- "Topmost" is the card not covered by any other card even though the
-- vertical display means it's closest to the bottom of the screen.
type Column = [Card]
topmost :: Column -> Maybe Card
topmost = headMay

showcols :: [Column] -> String
showcols cs = intercalate "\n" $ map (intercalate "  " . map show) (transpose cs)


data Tableau = Tableau [Cell] FlowerCell [Foundation] [Column]
instance Show Tableau where
  show (Tableau fcs f fs cs) =
       "F: " ++ unwords (map show fcs)
    ++ " Fl: "  ++ show f
    ++ " -> "   ++ unwords (map show fs)
    ++ "\n"     ++ showcols cs

type MoveCount = Int
data Game = Game Tableau MoveCount
  deriving (Show)

type ColumnIndex = Int
type CellIndex = Int

nextRank :: Rank -> Maybe Rank
nextRank One   = Just Two
nextRank Two   = Just Three
nextRank Three = Just Four
nextRank Four  = Just Five
nextRank Six   = Just Seven
nextRank Seven = Just Eight
nextRank Eight = Just Nine
nextRank Nine  = Nothing


nextCardForFoundation :: Foundation -> Maybe Card
nextCardForFoundation (Foundation suit (Suited s r:_)) = fmap (Suited suit) (nextRank r)
nextCardForFoundation (Foundation suit []) = Just (Suited suit One)

foundationBySuit :: Suit -> [Foundation] -> Foundation
foundationBySuit suit foundations = head $ filter (\(Foundation s _) -> s == suit) foundations

maybeIndex :: [a] -> Int -> Maybe a
maybeIndex [] _ = Nothing
maybeIndex as i
  | i < (length as - 1) = Just (as !! i)
  | otherwise = Nothing


data Move =
    MoveFromColumnToCell ColumnIndex CellIndex
  | MoveFromCellToColumn CellIndex ColumnIndex
  | BuildFromColumn ColumnIndex
  | BuildFromCell CellIndex
  | Pack ColumnIndex Card ColumnIndex
  | CollectDragons Suit
  deriving (Show)

mkBuildFromColumn :: Game -> ColumnIndex -> Maybe Move
mkBuildFromColumn (Game (Tableau _ _ foundations cs) _) i = do
  col <- maybeIndex cs i
  card <- topmost col
  next <- nextCardForFoundation (foundationBySuit (suitOf card) foundations)
  if card == next
      then return (BuildFromColumn i)
      else Nothing

mkBuildFromCell :: Game -> CellIndex -> Maybe Move
mkBuildFromCell (Game (Tableau fcs _ foundations _) _) i = do
  (Cell cell) <- maybeIndex fcs i
  card <- cell
  next <- nextCardForFoundation (foundationBySuit (suitOf card) foundations)
  if card == next
     then return (BuildFromCell i)
     else Nothing


deck :: Deck
deck = Flower : concatMap suitcards suits
  where
    suitcards suit = replicate 4 (Dragon suit) ++ map (Suited suit)
                     [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

tableau :: Deck -> Tableau
tableau deck = Tableau
             [Cell Nothing, Cell Nothing, Cell Nothing]
             (Cell Nothing) -- TODO FlowerCell?
             (map (\suit -> Foundation suit []) suits)
             (chunksOf 5 deck)

game d = Game d 0

move :: Game -> Move -> Game
move g m = undefined

main :: IO ()
main = do
  deal <- shuffleM deck
  print (tableau deal)
