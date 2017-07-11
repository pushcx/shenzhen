module Main where

import Data.List (concatMap, genericIndex, intercalate, transpose)
import Data.List.Split (chunksOf, endsWith, split)
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

-- A Run is at least one Sorted card, lowest Rank first, where each Card has
-- the next higher Rank and a different Suit than previous
type Run = [Card]

validRunPair :: Card -> Card -> Bool
validRunPair (Suited fs fr) (Suited ts tr) =
  case nextRank fr of
    (Just nr) -> fs /= ts && nr == tr
    Nothing   -> False
validRunPair _ _ = False

mkRunTo :: Column -> Card -> Maybe Run
mkRunTo [] card = Nothing
mkRunTo col card =
  case mayTakeTo col card of
    Nothing -> Nothing
    (Just possRun) ->
      if and (zipWith validRunPair possRun (tail possRun))
      then Just possRun
      else Nothing

mayTakeTo :: (Eq a) => [a] -> a -> Maybe [a]
mayTakeTo [] _ = Nothing
mayTakeTo cs card = halper (reverse cs) card
  where
    halper [] _ = Nothing
    halper (c:cs) card =
      if c == card
      then Just (reverse (c:cs))
      else halper cs card

validRunToCol :: Column -> Run -> Bool
validRunToCol [] _ = True
-- head's partial, but protected by line above
validRunToCol cs run = validRunPair (last run) (head cs)

showcols :: [Column] -> String
showcols cs = intercalate "\n" $ map (intercalate "  " . map show) (transpose cs)


data Tableau = Tableau [Cell] FlowerCell [Foundation] [Column]
instance Show Tableau where
  show (Tableau fcs f fs cs) =
       "F: " ++ unwords (map show fcs)
    ++ " Fl: "  ++ show f
    ++ " -> "   ++ unwords (map show fs)
    ++ "\n"     ++ showcols cs

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

mkBuildFromColumn :: Tableau -> ColumnIndex -> Maybe Move
mkBuildFromColumn (Tableau _ _ foundations cs) i = do
  col <- maybeIndex cs i
  card <- topmost col
  next <- nextCardForFoundation (foundationBySuit (suitOf card) foundations)
  if card == next
      then return (BuildFromColumn i)
      else Nothing

mkBuildFromCell :: Tableau -> CellIndex -> Maybe Move
mkBuildFromCell (Tableau fcs _ foundations _) i = do
  Left (Cell cell) <- maybeIndex fcs i
  card <- cell
  next <- nextCardForFoundation (foundationBySuit (suitOf card) foundations)
  if card == next
     then return (BuildFromCell i)
     else Nothing

mkPack :: Tableau -> ColumnIndex -> Card -> ColumnIndex -> Maybe Move
mkPack (Tableau _ _ _ cs) from card to = do
  fromCol <- maybeIndex cs from
  run <- mkRunTo fromCol card
  toCol <- maybeIndex cs to
  if validRunToCol toCol run
  then Just (Pack from card to)
  else Nothing



tableau :: Deck -> Tableau
tableau deck = Tableau
             [Cell Nothing, Cell Nothing, Cell Nothing]
             (Cell Nothing) -- TODO FlowerCell?
             (map (\suit -> Foundation suit []) suits)
             (chunksOf 5 deck)

testcol = [Suited Bamboo One, Suited Dot Two, Suited Bamboo Three, Suited Dot Four, Flower, Suited Bamboo Five]
deck :: Deck
deck = Flower : concatMap suitcards suits
  where
    suitcards suit = replicate 4 (Dragon suit) ++ map (Suited suit)
                    [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

move :: Tableau -> Move -> Tableau
move t m = undefined

main :: IO ()
main = do
  deal <- shuffleM deck
  print (tableau deal)
