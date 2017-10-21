module Main where

import Debug.Trace
import GHC.Stack (HasCallStack)

import Data.Foldable (asum)
import Data.List ((\\), concatMap, elemIndex, elemIndices, foldl', intercalate, sort, transpose)
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Safe (headMay)
import System.Random.Shuffle (shuffleM)

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers (NonEmptyList (..))


data Suit = Bamboo | Character | Dot
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

instance Show Card where
  show Flower             = "Fl"
  show (Dragon suit)      = "D" ++ show suit
  show (Suited suit rank) = show rank ++ show suit

newtype Deck = Deck [Card]
  deriving (Eq, Show)

suitOf :: Card -> Suit
suitOf (Suited s _) = s
suitOf (Dragon s) = s
suitOf Flower = error "Flower doesn't have a suit"

rankOf :: Card -> Rank
rankOf (Suited _ r) = r
rankOf (Dragon _) = error "Dragon doesn't have a rank"
rankOf Flower = error "Flower doesn't have a rank"


newtype Cell = Cell (Maybe Card)
  deriving (Eq, Ord)
type FlowerCell = Cell

instance Show Cell where
  show (Cell Nothing)  = "__"
  show (Cell (Just c)) = show c


data Foundation = Foundation Suit [Card]
  deriving (Eq, Ord)

mkFoundation :: Suit -> Foundation
mkFoundation s = Foundation s []

buildOnFoundation :: Foundation -> Card -> Maybe Foundation
buildOnFoundation f@(Foundation s cs) c = case nextCardForFoundation f of
                                          Nothing -> Nothing
                                          (Just n) -> if c == n
                                                         then Just (Foundation s (c:cs))
                                                         else Nothing

instance Show Foundation where
  show (Foundation suit []) = "_" ++ show suit
  show (Foundation _    cs) = show $ head cs


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
showcols cs = intercalate "\n" $ map (intercalate "  ") (transpose $ padShow cs)
  where
    longest = maximum $ map length cs
    padShow = map (\col -> reverse $ replicate (longest - length col) "  " ++ map show col)
newtype CollectedDragon = CollectedDragon Suit
  deriving (Eq, Ord, Show)


type DragonCell = Either Cell CollectedDragon

-- instance Show DragonCell where
--   show (Left c) = show c
--   show (Right (CollectedDragon s)) = "!" ++ show s

data Tableau = Tableau [DragonCell] FlowerCell [Foundation] [Column]
  deriving (Eq, Ord)
instance Show Tableau where
  show (Tableau cells f fs cs) =
       "C: " ++ unwords (map show cells)
    ++ " Fl: "  ++ show f
    ++ " -> "   ++ unwords (map show fs)
    ++ "\n"     ++ sc
    ++ replicate (12 - length (lines sc)) '\n'
      where sc = showcols cs
numCols :: Int
numCols = 7

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
nextRankForFoundation f = fmap rankOf (nextCardForFoundation f)

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
  deriving (Eq, Show)

mkMoveFromColumnToCell :: Tableau -> ColumnIndex -> CellIndex -> Maybe Move
mkMoveFromColumnToCell (Tableau cells _ _ cols) coli celli = do
  col <- maybeIndex cols coli
  card <- topmost col
  case card of
    Flower -> Nothing
    _ -> case maybeIndex cells celli of
           Nothing -> Nothing
           (Just (Right _)) -> Nothing
           (Just (Left (Cell Nothing))) -> Just (MoveFromColumnToCell coli celli)
           _ -> Nothing

mkMoveFromCellToColumn :: Tableau -> CellIndex -> ColumnIndex -> Maybe Move
mkMoveFromCellToColumn (Tableau cells _ _ cols) celli coli = do
  Left (Cell cell) <- maybeIndex cells celli
  card <- cell
  col <- maybeIndex cols coli
  case card of
    Flower -> Nothing
    _ -> case col of
           []    -> Just (MoveFromCellToColumn celli coli)
           (c:_) -> if validRunPair card c
                       then Just (MoveFromCellToColumn celli coli)
                       else Nothing

mkBuildFromColumn :: Tableau -> ColumnIndex -> Maybe Move
mkBuildFromColumn (Tableau _ _ foundations cs) i = do
  col <- maybeIndex cs i
  card@(Suited _ _) <- topmost col
  next <- nextCardForFoundation (foundationBySuit (suitOf card) foundations)
  if card == next
  then return (BuildFromColumn i)
  else Nothing

mkBuildFromCell :: Tableau -> CellIndex -> Maybe Move
mkBuildFromCell (Tableau cs _ foundations _) i = do
  Left (Cell cell) <- maybeIndex cs i
  card@(Suited _ _) <- cell
  next <- nextCardForFoundation (foundationBySuit (suitOf card) foundations)
  if card == next
  then return (BuildFromCell i)
  else Nothing

mkBuildFlower :: Tableau -> Maybe Move
mkBuildFlower (Tableau _ _ _ cols) =
  listToMaybe $ mapMaybe mayFlower [0..numCols]
  where
    mayFlower coli = do
      col <- maybeIndex cols coli
      Flower <- topmost col
      return (BuildFlower coli)

mkPack :: Tableau -> ColumnIndex -> Card -> ColumnIndex -> Maybe Move
mkPack _ _ Flower _ = Nothing
mkPack (Tableau _ _ _ cs) from card to =
  if from == to then Nothing else do
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
tableau (Deck deck) = Tableau
                      [Left (Cell Nothing), Left (Cell Nothing), Left (Cell Nothing)]
                      (Cell Nothing)
                      (map mkFoundation suits)
                      (chunksOf 5 deck)

standardCards :: [Card]
standardCards = sort $ Flower : concatMap suitcards suits
  where
    suitcards suit = replicate 4 (Dragon suit) ++ map (Suited suit)
                    [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

standardDeck :: Deck
standardDeck = Deck standardCards

replaceIndex :: Int -> a -> [a] -> [a]
replaceIndex i new list = take i list ++ new : drop (i + 1) list

buildCardToFoundations :: [Foundation] -> Card -> [Foundation]
buildCardToFoundations fs card = replaceIndex i (newF foundation) fs
  where
    suit = suitOf card
    foundation = foundationBySuit suit fs
    i = head $ elemIndices foundation fs
    newF f = case buildOnFoundation foundation card of
               (Just nf) -> nf
               Nothing -> error $ "Can't build " ++ show card ++ " to " ++ show f

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
applyM (Game st moves) m = Game st (moves ++ [m])

highestRankAutomaticallyBuildable :: [Foundation] -> Rank
highestRankAutomaticallyBuildable fos = minimum $ mapMaybe nextRankForFoundation fos

automaticallyBuildable :: Tableau -> Move -> Bool
automaticallyBuildable _ MoveFromColumnToCell{} = False
automaticallyBuildable _ MoveFromCellToColumn{} = False
automaticallyBuildable _ BuildFlower{} = True
automaticallyBuildable _ Pack{} = False
automaticallyBuildable _ CollectDragons{} = False
automaticallyBuildable (Tableau _ _ fos cols) (BuildFromColumn coli) =
  buildable fos (head $ cols !! coli)
automaticallyBuildable (Tableau cells _ fos _) (BuildFromCell celli) =
  buildable fos $ (\(Left (Cell (Just card))) -> card) $ cells !! celli

buildable :: [Foundation] -> Card -> Bool
buildable fos card@(Suited suit rank) =
  case nextCardForFoundation fo of
    Nothing -> False
    (Just nextCard) -> card == nextCard && rank <= highestRankAutomaticallyBuildable fos
  where
    fo = foundationBySuit suit fos

automaticBuildMove :: Tableau -> Maybe Move
automaticBuildMove tab =
  listToMaybe $ filter (automaticallyBuildable tab) $ maybeToList (mkBuildFlower tab) ++ (mapMaybe (mkBuildFromCell tab) [0..2] ++ mapMaybe (mkBuildFromColumn tab) [0..numCols])

automaticBuild :: Tableau -> Tableau
automaticBuild t = case automaticBuildMove t of
                   Nothing -> t
                   (Just b) -> automaticBuild $ applyT t b

won :: Game -> Bool
won g = allColsFinished $ current g
  where allColsFinished (Tableau _ _ _ cells) = all (== []) cells

possibleMoves :: Tableau -> [Move]
possibleMoves tab@(Tableau _ _ _ cols) = catMaybes $
  [mkCollectDragons tab suit | suit <- suits] ++
  [mkBuildFromColumn tab coli | coli <- [0..numCols]] ++
  [mkBuildFromCell tab celli | celli <- [0..2]] ++
  [mkPack tab fromi card toi | fromi <- [0..numCols], card <- reverse $ lastCardsOfRuns (cols !! fromi), toi <- [0..numCols]] ++
  [mkMoveFromCellToColumn tab celli coli | celli <- [0..2], coli <- [0..numCols]] ++
  [mkMoveFromColumnToCell tab coli celli | celli <- [0..2], coli <- [0..numCols]]


-- moves are oldest-first
data Game = Game Tableau [Move]
  deriving (Show)
-- instance Show Game where
--   show (Game t ms) = (show length ms) ++ " " ++ show t ++ "\n" ++ ms

moveCount (Game _ ms) = length ms


game :: Tableau -> Game
game t = Game t []

current :: Game -> Tableau
current (Game start moves) = foldl' applyT start moves

previous :: Game -> [Tableau]
previous (Game start moves) = scanl applyT start moves

-- novelPossibleMoves :: Losses -> Game -> [Move]
-- novelPossibleMoves losses game = novel
--   where
--     now = current game
--     moves = possibleMoves now
--     seen = previous game
--     possibleNexts = map (applyT now) moves
--     paired = zip moves possibleNexts
--     novel = map fst $ filter (\(_, tab) -> tab `Set.notMember` losses && tab `notElem` seen) pair

-- lost :: Losses -> Game -> Bool
-- lost losses g = not (won g) && null (novelPossibleMoves losses g)

type Losses = Set.Set Tableau

data Outcome = Unknown | Lost | Won
type Results = Map.Map Tableau Outcome

outcome :: Game -> Maybe Game
outcome = undefined


--outcomes :: Either Losses Game -> Game -> Either Losses Game
--outcomes (Right g) _ = Right g
--outcomes (Left l) g
--  | trace (show (current g)) won g = Right g
--  | lost l g = Left $ Set.insert (current g) l
--  | otherwise = run l g

outcomes :: Losses -> Game -> Either Losses (Game, Losses)
outcomes ls g
  | won g = Right (g, ls)
  | otherwise = bar ls moves
  where
    seen = previous g -- [Tableau]
    now = last seen
    moves = possibleMoves (trace (show (moveCount g) ++ " " ++ show now) now)
    foo :: Losses -> Tableau -> Bool
    foo ls' tab = tab `Set.notMember` ls' && tab `notElem` seen
    bar :: Losses -> [Move] -> Either Losses (Game, Losses)
    bar ls' [] = Left ls'
    bar ls' (m:ms) = if foo ls' next
                    then case outcomes ls' (applyM g m) of
                           Left ls'' -> bar (Set.insert next ls'') ms
                           Right (g', ls'') -> Right (g', ls'')
                    else bar ls' ms
                      where
                        next = applyT now m

-- outcomes (Right g) _ = Right g
-- outcomes (Left ls) g
--   | trace (show (current g)) won g = Right g
--   | lost ls g = Left $ Set.insert (current g) ls
--   | otherwise = Left ls

-- run :: Losses -> Game -> Either Losses Game
-- run l g = foldl' outcomes (Left l) (map (applyM g) $ novelPossibleMoves l g)

-- run :: Losses -> Game -> Either Losses Game
-- run ls g = case outcomes ls g of
--              Right _ -> Right g
--              Left ls' -> if lost ls' g
--                             then Left ls'
--                             else run ls' g


--outcome g
----  | not $ prop_standardCards (tabCards $ current g) = error ("lost/extra cards! \n" ++ show (current g))
-- | won g = Just g
-- | lost g = Nothing
-- | otherwise = trace ("\n" ++ (show $ current g)) $ asum $ map (outcome . applyM g) (novelPossibleMoves g)

deal :: Deck -> IO Deck
deal (Deck d) = do
  cs <- shuffleM d
  return $ Deck cs

main :: IO ()
main = do
  d <- deal standardDeck
  let st = tableau d
  print st
  let g = game st
  case outcomes Set.empty g of
    (Right (Game t ms, l)) -> putStrLn ( show t ++ show ms ++ "\nmoves: " ++ show (length ms) ++ " + losses: " ++ show (length l))
    (Left losses) -> putStrLn $ "lost " ++ show (length losses)

cards :: Deck -> [Card]
cards (Deck cs) = cs

instance Arbitrary Card where
  arbitrary = elements $ cards standardDeck

instance Arbitrary Deck where
  arbitrary = do
    cs <- shuffle (cards standardDeck)
    return $ Deck cs

instance Arbitrary Tableau where
  arbitrary = do
    deck <- arbitrary
    return $ tableau deck

instance Arbitrary Game where
  arbitrary = do
    tab <- arbitrary
    return $ game tab

newtype TableauMove = TableauMove (Tableau, Move)
  deriving (Show)

instance Arbitrary TableauMove where
  arbitrary = do
    t <- arbitrary
    m <- elements $ possibleMoves t
    return $ TableauMove (t, m)

tabCards :: Tableau -> [Card]
tabCards (Tableau cells fl fo cols) =
  concatMap cellCard cells ++ flowerCard fl ++ concatMap foundationCards fo ++ concat cols
  where
    cellCard (Right (CollectedDragon suit)) = replicate 4 (Dragon suit)
    cellCard (Left (Cell (Just card))) = [card]
    cellCard (Left (Cell Nothing)) = []
    flowerCard (Cell (Just Flower)) = [Flower]
    flowerCard (Cell (Just _)) = error "non-flower on flower cell"
    flowerCard (Cell Nothing) = []
    foundationCards (Foundation _ cs) = cs

prop_standardCards :: [Card] -> Bool
prop_standardCards cs = sort cs == standardCards

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

  describe "prop_standardCards" $
    it "has the same 40 cards as the starting deck" $
      property $ \(Deck cs) -> prop_standardCards cs

  describe "new games" $ do
    it "has the same 40 cards as the starting deck" $
      property $ \t -> prop_standardCards $ tabCards t
    it "always has the standard deck after a move" $
      property $ \(TableauMove (t, m) ) -> prop_standardCards $ tabCards (applyT t m)

  describe "mkBuildFromColumn" $
    it "will build Flowers" $
      mkBuildFlower (tableau (Deck [Flower])) `shouldBe` Just (BuildFlower 0)

  describe "automaticBuildMove" $
    it "can build Flowers" $
      automaticBuildMove (tableau (Deck [Flower])) `shouldBe` Just (BuildFlower 0)
