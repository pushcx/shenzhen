module Main where

import Debug.Trace
import GHC.Stack (HasCallStack)

import Data.Foldable (asum)
import Data.List ((\\), concatMap, elemIndex, elemIndices, foldl', intercalate, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Safe (headMay)
import System.Random.Shuffle (shuffleM)

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers (NonEmptyList (..))


data Suit = Bamboo | Character | Dot
  deriving (Eq)

suits :: [Suit]
suits = [Bamboo, Character, Dot]

instance Show Suit where
  show Bamboo    = "b"
  show Character = "c"
  show Dot       = "d"


data Rank = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq, Ord)

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
suitOf Flower = error "Flower doesn't have a suit"

rankOf :: Card -> Rank
rankOf (Suited _ r) = r
rankOf (Dragon _) = error "Dragon doesn't have a rank"
rankOf Flower = error "Flower doesn't have a rank"


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

-- finds all Runs in a column: a 3-card run must have a 2 and 1 card
-- that can be taken independently
lastCardsOfRuns :: Column -> [Card]
lastCardsOfRuns [] = []
lastCardsOfRuns [c] = [c]
lastCardsOfRuns col = head col : map snd (takeWhile fst $ zipWith (\c d -> (validRunPair c d, d)) col (tail col))

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

-- returns the sublist up to and including the first instance of the target
-- element
mayTakeTo :: (Eq a) => [a] -> a -> Maybe [a]
mayTakeTo list e = fmap (\i -> take (i + 1) list) (elemIndex e list)

unsafeTakeTo :: (Eq a) => [a] -> a -> [a]
unsafeTakeTo list e = case mayTakeTo list e of
                        Nothing -> error "element not in list, I warned you I was unsafe"
                        (Just sublist) -> sublist

validRunToCol :: Column -> Run -> Bool
validRunToCol [] _ = True
-- head's partial, but protected by line above
validRunToCol cs run = validRunPair (last run) (head cs)

showcols :: [Column] -> String
showcols cs = intercalate "\n" $ map (intercalate "  " . map show) (transpose $ map reverse cs)

newtype CollectedDragon = CollectedDragon Suit
  deriving (Show, Eq)


type DragonCell = Either Cell CollectedDragon

data Tableau = Tableau [DragonCell] FlowerCell [Foundation] [Column]
  deriving (Eq)
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

nextRankForFoundation :: Foundation -> Maybe Rank
nextRankForFoundation (Foundation _ (Suited _ r:_)) = nextRank r
nextRankForFoundation (Foundation _ [])             = Just One
nextRankForFoundation (Foundation _ _) = error "Non-Suited card on Foundation"

nextCardForFoundation :: Foundation -> Maybe Card
nextCardForFoundation (Foundation suit (Suited _ r:_)) = fmap (Suited suit) (nextRank r)
nextCardForFoundation (Foundation suit []) = Just (Suited suit One)
nextCardForFoundation (Foundation _ _) = error "Non-Suited card on Foundation"

foundationBySuit :: Suit -> [Foundation] -> Foundation
foundationBySuit suit foundations = head $ filter (\(Foundation s _) -> s == suit) foundations

maybeIndex :: [a] -> Int -> Maybe a
maybeIndex [] _ = Nothing
maybeIndex as i
  | i <= (length as - 1) = Just (as !! i)
  | otherwise = Nothing


data Move =
    MoveFromColumnToCell ColumnIndex CellIndex
  | MoveFromCellToColumn CellIndex ColumnIndex
  | BuildFromColumn ColumnIndex
  | BuildFromCell CellIndex
  | BuildFlower ColumnIndex
  | Pack ColumnIndex Card ColumnIndex
  | CollectDragons Suit
  deriving (Show)

mkMoveFromColumnToCell :: Tableau -> ColumnIndex -> CellIndex -> Maybe Move
mkMoveFromColumnToCell (Tableau cells _ _ cols) coli celli = do
  col <- maybeIndex cols coli
  _ <- topmost col -- only care that there is a card, not what it is
  case maybeIndex cells celli of
    Nothing -> Nothing
    (Just (Right _)) -> Nothing
    (Just (Left (Cell Nothing))) -> Just (MoveFromColumnToCell coli celli)
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

mkBuildFlower :: Tableau -> Maybe Move
mkBuildFlower (Tableau _ _ _ cols) =
  listToMaybe $ mapMaybe mayFlower [0..7]
  where
    mayFlower coli = do
      col <- maybeIndex cols coli
      Flower <- topmost col
      Just (BuildFlower coli)

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
             (Cell Nothing)
             (map (\suit -> Foundation suit []) suits)
             (chunksOf 5 deck)

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

takeCardFromCell :: [DragonCell] -> Int -> (Card, [DragonCell])
takeCardFromCell cells i = (card, newCells)
  where
    Left (Cell cell) = cells !! i
    (Just card) = cell
    newCells = replaceIndex i (Left (Cell Nothing)) cells

takeCardFromCol :: [Column] -> Int -> (Card, [Column])
takeCardFromCol cols i = (card, newCols)
  where
    fromCol = cols !! i
    card = head fromCol
    newCol = drop 1 fromCol
    newCols = replaceIndex i newCol cols

-- applys a player's move and automatically builds if possible
applyT :: Tableau -> Move -> Tableau
applyT t m = automaticBuild (applied t m)
  where
    applied (Tableau cells fl fo cols) (MoveFromColumnToCell coli celli) = Tableau newCells fl fo newCols
      where
        (card, newCols) = takeCardFromCol cols coli
        newCell = Left (Cell (Just card))
        newCells = replaceIndex celli newCell cells
    applied (Tableau cells fl fo cols) (MoveFromCellToColumn celli coli) = Tableau newCells fl fo newCols
      where
        (card, newCells) = takeCardFromCell cells celli
        oldCol = cols !! coli
        newCol = card : oldCol
        newCols = replaceIndex coli newCol cols
    applied (Tableau cells fl fo cols) (BuildFromColumn coli) = Tableau cells fl newFs newCols
      where
        (card, newCols) = takeCardFromCol cols coli
        newFs = buildCardToFoundations fo card
    applied (Tableau cells fl fo cols) (BuildFromCell celli) = Tableau newCells fl newFs cols
      where
        (card, newCells) = takeCardFromCell cells celli
        newFs = buildCardToFoundations fo card
    applied (Tableau cells _ fos cols) (BuildFlower coli) = Tableau cells (Cell $ Just card) fos newCols
      where
        (card, newCols) = takeCardFromCol cols coli
    applied (Tableau cells fl fo cols) (Pack fromi card toi) = Tableau cells fl fo newCols
      where
        fromCol = cols !! fromi
        toCol = cols !! toi
        run = unsafeTakeTo fromCol card
        shortenedFromCol = drop (length run) fromCol
        lengthenedToCol = run ++ toCol
        newCols = replaceIndex fromi shortenedFromCol (replaceIndex toi lengthenedToCol cols)
    applied (Tableau cells fl fo cols) (CollectDragons suit) = Tableau newcells fl fo newcols
      where
        newcells = addCollectedDragonsToCells (cellsWithoutDragons cells suit) suit
        newcols = colsWithoutDragons cols suit

applyM :: Game -> Move -> Game
applyM g@(Game st moves) m = Game (applyT (current g) m) (moves ++ [m])

highestRankAutomaticallyBuildable :: [Foundation] -> Rank
highestRankAutomaticallyBuildable fos = minimum $ mapMaybe nextRankForFoundation fos

automaticallyBuildable :: [Foundation] -> Card -> Bool
automaticallyBuildable fos card@(Suited suit rank) =
  case nextCardForFoundation fo of
    Nothing -> False
    (Just nextCard) -> card == nextCard && rank <= highestRankAutomaticallyBuildable fos
  where
    fo = foundationBySuit suit fos
automaticallyBuildable _ Flower = error "shouldn't be trying to build Flower"
automaticallyBuildable _ (Dragon _) = error "shouldn't be trying to build Dragon"

cardBuilt :: Tableau -> Move -> Card
cardBuilt (Tableau _ _ _ cols) (BuildFromColumn coli) = head $ cols !! coli
cardBuilt (Tableau cells _ _ _) (BuildFromCell celli) = (\(Left (Cell (Just card))) -> card) $ cells !! celli
cardBuilt _ (BuildFlower _) = Flower
cardBuilt _ _ = error "No card built for this move"

automaticBuildMove :: Tableau -> Maybe Move
automaticBuildMove tab@(Tableau _ _ fos _) =
  listToMaybe $ filter (automaticallyBuildable fos . cardBuilt tab) $ maybeToList (mkBuildFlower tab) ++ (mapMaybe (mkBuildFromCell tab) [0..2] ++ mapMaybe (mkBuildFromColumn tab) [0..7])

automaticBuild :: Tableau -> Tableau
automaticBuild t = case automaticBuildMove t of
                   Nothing -> t
                   (Just b) -> automaticBuild $ applyT t b

won :: Game -> Bool
won g = allColsFinished $ current g
  where allColsFinished (Tableau _ _ _ cells) = all (== []) cells

possibleMoves :: Tableau -> [Move]
possibleMoves tab@(Tableau _ _ _ cols) = catMaybes $
  [mkMoveFromColumnToCell tab coli celli | celli <- [0..2], coli <- [0..7]] ++
  [mkMoveFromCellToColumn tab celli coli | celli <- [0..2], coli <- [0..7]] ++
  [mkBuildFromColumn tab coli | coli <- [0..7]] ++
  [mkBuildFromCell tab celli | celli <- [0..2]] ++
  [mkPack tab fromi card toi | fromi <- [0..7], card <- lastCardsOfRuns (cols !! fromi), toi <- [0..7]] ++
  [mkCollectDragons tab suit | suit <- suits]


data Game = Game Tableau [Move]
  deriving (Show)


game :: Tableau -> Game
game t = Game t []

current :: Game -> Tableau
current (Game start moves) = foldl' applyT start moves

previous :: Game -> [Tableau]
previous (Game start moves) = scanl applyT start moves

novelPossibleMoves :: Game -> [Move]
novelPossibleMoves game = novel
  where
    now = current game
    moves = possibleMoves now
    seen = previous game
    possibleNexts = map (applyT now) moves
    paired = zip moves possibleNexts
    novel = map fst $ filter (\(_, tab) -> tab `notElem` seen) paired

lost :: Game -> Bool
lost game = not (won game) && null (novelPossibleMoves game)


-- Nothing == Unwinnable, Just == Winnable
type Outcome = Maybe Game

outcome :: Game -> Outcome
outcome g
  | won g = Just g
  | lost g = Nothing
  | otherwise = asum $ map (outcome . applyM g) (novelPossibleMoves g)

main :: IO ()
main = do
  deal <- shuffleM standardDeck
  let st = tableau deal
  print st
  let g = game st
  print (outcome g)



instance Arbitrary Card where
  arbitrary = elements standardDeck

test :: IO ()
test = hspec $ do
  describe "headMay" $ do
    it "returns the top card of the Column" $
      headMay [Dragon Bamboo, Suited Dot Three] `shouldBe` Just (Dragon Bamboo)
    it "is Nothing on an empty Column" $
      headMay ([] :: Column) `shouldBe` Nothing

  describe "mayTakeTo" $ do
    it "takes up to desired card" $
      mayTakeTo [Dragon Bamboo, Suited Dot Three] (Suited Dot Three) `shouldBe` Just [Dragon Bamboo, Suited Dot Three]
    it "can take just one card" $
      mayTakeTo [Dragon Bamboo, Suited Dot Three] (Dragon Bamboo) `shouldBe` Just [Dragon Bamboo]
    it "is Nothing if card is not found" $
      mayTakeTo [Dragon Bamboo, Suited Dot Three] (Dragon Dot) `shouldBe` Nothing
    it "takes up until first if there are duplicates" $
      mayTakeTo [Dragon Dot, Suited Dot Two, Dragon Dot] (Dragon Dot) `shouldBe` Just [Dragon Dot]
    it "takes the correct amount of Cards" $
      property $ \col c -> let m = mayTakeTo (col :: [Card]) (c :: Card) in
                               fmap length m == fmap (+1) (elemIndex c col)
    it "uses Nothing, not empty list, if card not found" $
      property $ \col c -> let m = mayTakeTo (col :: [Card]) (c :: Card) in
                               m /= Just []

  describe "lastCardsOfRuns" $
    it "finds no Runs in an empty list" $
      lastCardsOfRuns [] `shouldBe` []
