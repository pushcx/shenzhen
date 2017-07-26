module Main where

import Data.List (concatMap, elemIndices, intercalate, transpose)
import Data.List.Split (chunksOf)
import Safe (headMay)
import System.Random.Shuffle (shuffleM)

data Suit = Bamboo | Character | Dot
  deriving (Eq)

suits :: [Suit]
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
suitOf _ = error "Flower doesn't have a suit"

rankOf :: Card -> Rank
rankOf (Suited _ r) = r
rankOf _ = error "Flower and Dragon cards don't have a suit"


newtype Cell = Cell (Maybe Card)
  deriving (Eq)
type FlowerCell = Cell

instance Show Cell where
  show (Cell Nothing)  = "__"
  show (Cell (Just c)) = show c


data Foundation = Foundation Suit [Card]
  deriving (Eq)

instance Show Foundation where
  show (Foundation suit []) = "_" ++ show suit
  show (Foundation _    cs) = show . head $ reverse cs


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
mkRunTo [] _ = Nothing
mkRunTo col card =
  case mayTakeTo col card of
    Nothing -> Nothing
    (Just possRun) ->
      if and (zipWith validRunPair possRun (tail possRun))
      then Just possRun
      else Nothing

mayTakeTo :: (Eq a) => [a] -> a -> Maybe [a]
mayTakeTo [] _ = Nothing
mayTakeTo collection target = halper (reverse collection) target
  where
    halper [] _ = Nothing
    halper (c:cs) card =
      if c == card
      then Just (reverse (c:cs))
      else halper cs card

unsafeTakeTo :: (Eq a) => [a] -> a -> [a]
unsafeTakeTo [] _ = error "element not in list, I warned you I was unsafe"
unsafeTakeTo (c:cs) card =
  if c == card
  then reverse (c:cs)
  else unsafeTakeTo cs card

validRunToCol :: Column -> Run -> Bool
validRunToCol [] _ = True
-- head's partial, but protected by line above
validRunToCol cs run = validRunPair (last run) (head cs)

showcols :: [Column] -> String
showcols cs = intercalate "\n" $ map (intercalate "  " . map show) (transpose cs)

newtype CollectedDragon = CollectedDragon Suit
  deriving (Show, Eq)


type DragonCell = Either Cell CollectedDragon

data Tableau = Tableau [DragonCell] FlowerCell [Foundation] [Column]
instance Show Tableau where
  show (Tableau cells f fs cs) =
       "C: " ++ unwords (map show cells)
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
nextRank Five  = Just Six
nextRank Six   = Just Seven
nextRank Seven = Just Eight
nextRank Eight = Just Nine
nextRank Nine  = Nothing


nextCardForFoundation :: Foundation -> Maybe Card
nextCardForFoundation (Foundation suit (Suited _ r:_)) = fmap (Suited suit) (nextRank r)
nextCardForFoundation (Foundation suit []) = Just (Suited suit One)
nextCardForFoundation (Foundation _ _) = error "Non-Suited card on Foundation"

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

mkMoveFromColumnToCell :: Tableau -> ColumnIndex -> CellIndex -> Maybe Move
mkMoveFromColumnToCell (Tableau cells _ _ cols) coli celli = do
  col <- maybeIndex cols coli
  _ <- topmost col -- only care that there is a card, not what it is
  case maybeIndex cells celli of
    Nothing -> Just (MoveFromColumnToCell coli celli)
    _ -> Nothing

mkMoveFromCellToColumn :: Tableau -> CellIndex -> ColumnIndex -> Maybe Move
mkMoveFromCellToColumn (Tableau cells _ _ cols) celli coli = do
  Left (Cell cell) <- maybeIndex cells celli
  card <- cell
  col <- maybeIndex cols coli
  case col of
    []    -> Just (MoveFromCellToColumn celli coli)
    (c:_) -> if validRunPair card c
                then Just (MoveFromCellToColumn celli coli)
                else Nothing

mkBuildFromColumn :: Tableau -> ColumnIndex -> Maybe Move
mkBuildFromColumn (Tableau _ _ foundations cs) i = do
  col <- maybeIndex cs i
  card <- topmost col
  next <- nextCardForFoundation (foundationBySuit (suitOf card) foundations)
  if card == next
  then return (BuildFromColumn i)
  else Nothing

mkBuildFromCell :: Tableau -> CellIndex -> Maybe Move
mkBuildFromCell (Tableau cs _ foundations _) i = do
  Left (Cell cell) <- maybeIndex cs i
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

mkCollectDragons :: Tableau -> Suit -> Maybe Move
mkCollectDragons t s =
  if allDragonsAvailable t s && cellOpenFor t s
  then Just (CollectDragons s)
  else Nothing
    where
      allDragonsAvailable :: Tableau -> Suit -> Bool
      allDragonsAvailable (Tableau cells _ _ cols) suit =
        countDragonsInCells cells suit + countDragonsInCols cols suit == 4
      cellOpenFor (Tableau cells _ _ _) suit =
        not . null $ filter (\c -> c == Left (Cell (Just (Dragon suit))) || c == Left (Cell Nothing)) cells

countDragonsInCells :: [DragonCell] -> Suit -> Int
countDragonsInCells cells suit = length $ filter (== Left (Cell (Just (Dragon suit)))) cells

countDragonsInCols :: [Column] -> Suit -> Int
countDragonsInCols cols suit   = length $ filter (== Just (Dragon suit)) (map topmost cols)

dragonsInCells :: [DragonCell] -> Suit -> [Card]
dragonsInCells cells suit = replicate (countDragonsInCells cells suit) (Dragon suit)

dragonsInCols :: [Column] -> Suit -> [Card]
dragonsInCols cols suit = replicate (countDragonsInCols cols suit) (Dragon suit)

cellWithoutDragons :: DragonCell -> Suit -> DragonCell
cellWithoutDragons c@(Left (Cell (Just (Dragon suit)))) s =
  if suit == s
     then Left (Cell Nothing)
     else c
cellWithoutDragons anything _ = anything

cellsWithoutDragons :: [DragonCell] -> Suit -> [DragonCell]
cellsWithoutDragons cells suit = map (`cellWithoutDragons` suit) cells

addCollectedDragonsToCells :: [DragonCell] -> Suit -> [DragonCell]
addCollectedDragonsToCells [] _ = error "what? no empty cell"
addCollectedDragonsToCells (Left (Cell Nothing):cs) s = Right (CollectedDragon s) : cs
addCollectedDragonsToCells (c:cs) s = c : addCollectedDragonsToCells cs s

colWithoutDragons :: Column -> Suit -> Column
colWithoutDragons [] _ = []
colWithoutDragons col@(c:cs) s
  | c == Dragon s = cs
  | otherwise = col

colsWithoutDragons :: [Column] -> Suit -> [Column]
colsWithoutDragons cells suit = map (`colWithoutDragons` suit) cells

tableau :: Deck -> Tableau
tableau deck = Tableau
             [Left (Cell Nothing), Left (Cell Nothing), Left (Cell Nothing)]
             (Cell Nothing) -- TODO FlowerCell?
             (map (\suit -> Foundation suit []) suits)
             (chunksOf 5 deck)

testcol :: Column
testcol = [Suited Bamboo One, Suited Dot Two, Suited Bamboo Three, Suited Dot Four, Flower, Suited Bamboo Five]
standardDeck :: Deck
standardDeck = Flower : concatMap suitcards suits
  where
    suitcards suit = replicate 4 (Dragon suit) ++ map (Suited suit)
                    [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

replaceIndex :: Int -> a -> [a] -> [a]
replaceIndex i new list = take i list ++ new : drop (i + 1) list

buildCardToFoundations :: [Foundation] -> Card -> [Foundation]
buildCardToFoundations fo card = replaceIndex i (newF foundation) fo
  where
    suit = suitOf card
    foundation = foundationBySuit suit fo
    i = head $ elemIndices foundation fo
    newF (Foundation _ cards) = Foundation suit (card : cards)

move :: Tableau -> Move -> Tableau
move (Tableau cells fl fo cols) (MoveFromColumnToCell coli celli) = undefined
move (Tableau cells fl fo cols) (MoveFromCellToColumn celli coli) = undefined
move (Tableau cells fl fo cols) (BuildFromColumn coli) = Tableau cells fl newFs newCols
  where
    fromCol = cols !! coli
    card = head fromCol
    newCol = drop 1 fromCol
    newCols = replaceIndex coli newCol cols
    newFs = buildCardToFoundations fo card
move (Tableau cells fl fo cols) (BuildFromCell celli) = Tableau newCells fl newFs cols
  where
    Left (Cell cell) = cells !! celli
    (Just card) = cell
    newCells = replaceIndex celli (Left (Cell Nothing)) cells
    newFs = buildCardToFoundations fo card
move (Tableau cells fl fo cols) (Pack fromi card toi) = Tableau cells fl fo newCols
  where
    fromCol = cols !! fromi
    toCol = cols !! toi
    run = unsafeTakeTo fromCol card
    shortenedFromCol = drop (length run) fromCol
    lengthenedToCol = run ++ toCol
    newCols = replaceIndex fromi shortenedFromCol (replaceIndex toi lengthenedToCol cols)

move (Tableau cells fl fo cols) (CollectDragons suit) = Tableau newcells fl fo newcols
  where
    newcells = addCollectedDragonsToCells (cellsWithoutDragons cells suit) suit
    newcols = colsWithoutDragons cols suit

main :: IO ()
main = do
  deal <- shuffleM standardDeck
  print (tableau deal)
