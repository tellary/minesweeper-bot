{-# LANGUAGE RecordWildCards   #-}

module Bot where

import Control.Lens  hiding (element)
import Control.Monad (join)
import Data.Foldable (fold, toList)
import Data.List     (find, group, nub, transpose)
import Field
import Game

neighbors field pos =
  [ fieldAt field (Position x' y')
  | dx <- [-1..1], dy <- [-1..1]
  , (dx, dy) /= (0, 0)
  , let x' = x pos + dx, let y' = y pos + dy
  , x' >= 0, y' >= 0, x' < fieldWidth, y' < fieldHeight
  ]
  where
    FieldSize {..} = fieldSize field
-- f = mkField [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- neighbors f (Position 0 0) == [4,2,5]
-- neighbors f (Position 2 2) == [5,8,6]
-- neighbors f (Position 1 1) [1,4,7,2,8,3,6,9]

markPosIfMatchNumber :: CellField -> Position -> [Position]
markPosIfMatchNumber field pos
  = case cell of
      Cell (Number n) _
        | length flagCells + length fieldCells == n
          -> map Field.pos fieldCells
      _ -> []
  where
    cell = cellAt field pos
    flagCells  = filter isFlag  (neighbors field pos)
    fieldCells = filter isField (neighbors field pos)

digPosIfMatchNumber :: CellField -> Position -> Bool
digPosIfMatchNumber field pos
  = case cell of
      Cell (Number n) _ -> length flagCells == n && not (null fieldCells)
      _ -> False
  where
    cell = cellAt field pos
    flagCells  = filter isFlag  (neighbors field pos)
    fieldCells = filter isField (neighbors field pos)

markIfMatchNumber :: CellField -> [Position]
markIfMatchNumber field
  = fold . fmap (markPosIfMatchNumber field . pos) $ field

digIfMatchNumber :: CellField -> [Position]
digIfMatchNumber field
  = filter (digPosIfMatchNumber field) . map pos . toList $ field
-- :l Bot GoogleMinesweeper
-- import GoogleMinesweeper
-- r <- returnSession remoteConfig (openField Easy)
-- markIfMatchNumber (snd r) (Position 0 0)
-- import Test.WebDriver
-- f <- runWD (fst r) readFieldFromScreen
-- markIfMatchNumber f (Position 0 0)

type NumItems = Int
type NumPositions = Int

combinations :: NumItems -> NumPositions -> [[Bool]]
combinations _ 0 = [[]]
combinations 0 _ = [[]]
combinations 1 1 = [[True]]
combinations 1 m
  = (True : (take (m - 1) $ repeat False))
  : (map (False:) $ combinations 1 (m - 1))
combinations n m
  | n >  m = [[]]
  | n == m = [take n $ repeat True]
  | otherwise
  =  (map (True :) $ combinations (n - 1) (m - 1))
  ++ (map (False:) $ combinations  n      (m - 1))

neighborsCombinations field pos n
  = map (map fst . filter snd . zip _neighbors) _combinations
  where
    _neighbors = neighbors field pos
    _combinations = combinations n (length _neighbors)
-- f = mkField [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- neighborsCombinations f (Position 0 0) 2 == [[4,2],[4,5],[2,5]]

flagableFieldCombinations :: CellField -> Position -> [[Cell]]
flagableFieldCombinations field pos
  = case cellAt field pos of
      (Cell (Number n) _)
        -> map (map (\cell -> cell { cellType = Flag }))
           . filter (null . find (not . isField))
           $ _neighborCombinations
        where
          _neighbors = neighbors field pos
          _flagCells  = length $ filter isFlag _neighbors
          _combinations = combinations (n - _flagCells) (length _neighbors)
          _neighborCombinations
            = map (map fst . filter snd . zip _neighbors) _combinations
      _ -> [[]]


positionsField n m
  = mkField
  [ [ Position x y | x <- [0 .. n - 1] ]
    | y <- [0 .. m - 1]
  ]
testFieldTypes1
  = mkField
  [ [Field   , Field   , Field   ]
  , [Number 1, Number 2, Number 1]
  , [Field   , Open    , Open    ]
  ]
testField1 = Cell <$> testFieldTypes1 <*> positionsField 3 3
-- pPrintNoColor $ flagableFieldCombinations testField1 (Position 1 1)

overrideCells = overrideCellsIf (const True)
overrideCellsIf p override cells = do
  cell <- cells
  let overrideMaybe = find (\cell' -> pos cell == pos cell') override
  return $ maybe cell (\cell' -> if p cell' then cell' else cell) overrideMaybe
-- pPrint $ overrideCells (flagableFieldCombinations testField1 (Position 1 1) !! 1) (neighbors testField1 (Position 2 1))

areFlagsPossibleOnCell field flags (Cell (Number n) pos)
  = _newNumberOfFlags <= n
  where
    _neighbors = neighbors field pos
    _overrideCells = overrideCells flags _neighbors
    _newNumberOfFlags = length . filter isFlag $ _overrideCells
areFlagsPossibleOnCell _ _ _ = error "areFlagsPossibleOnCell: must be number"
-- areFlagsPossibleOnCell testField1 (flagableFieldCombinations testField1 (Position 1 1) !! 1) (cellAt testField1 (Position 2 1))
-- areFlagsPossibleOnCell testField1 (flagableFieldCombinations testField1 (Position 1 1) !! 5) (cellAt testField1 (Position 2 1))

areFlagsPossibleOnAllNeighbors field pos flags
  = all _flagsPossibleOnNeighbor _numbers
  where
    _neighbors = neighbors field pos
    _numbers = filter isNumber _neighbors
    _flagsPossibleOnNeighbor = areFlagsPossibleOnCell field flags
-- pPrint $ filter (areFlagsPossibleOnAllNeighbors testField1 (Position 1 1)) $ flagableFieldCombinations testField1 (Position 1 1)

markPosIfPossibleInAllCombinations field pos
  = join
    . map (map head)
    . filter ((==1) . length)
    . map group
    . transpose
    . map (map Field.pos)
    . filter (areFlagsPossibleOnAllNeighbors field pos)
    $ flagableFieldCombinations field pos

markIfPossibleInAllCombinations :: CellField -> [Position]
markIfPossibleInAllCombinations field
  = fold . fmap (markPosIfPossibleInAllCombinations field . pos) $ field

mark :: Game -> ([Position], Game)
mark game
  = ( markPositions
    , game & flagsLeft .~ game^.flagsLeft - length markPositions
    )
  where
    markPositions = nub
      $  markIfMatchNumber (game^.field)
      ++ markIfPossibleInAllCombinations (game^.field)

testFieldTypes2
  = mkField
  [ [Field   , Field   , Field   , Field   ]
  , [Number 1, Number 1, Number 1, Number 1]
  , [Open    , Open    , Open    , Open    ]
  ]
testField2 = Cell <$> testFieldTypes2 <*> positionsField 4 3
-- pPrint $ flagableFieldCombinations testField2 (Position 1 1)
-- pPrint $ overrideCells (flagableFieldCombinations testField2 (Position 1 1) !! 1) (neighbors testField2 (Position 2 1))

openAroundOneCellIfMatchNumber :: CellField -> Cell -> Maybe CellField
openAroundOneCellIfMatchNumber field (Cell (Number n) pos)
  | length flagCells == n && length fieldCells > 0
  = Just $ foldl
    (\field fieldCell
      -> updateCell field fieldCell { cellType = OpenUnknown }
    )
    field
    fieldCells
  | otherwise = Nothing
  where
    _neighbors = neighbors field pos
    flagCells  = filter isFlag  _neighbors
    fieldCells = filter isField _neighbors
openAroundOneCellIfMatchNumber _ _ = error "openPosIfMatchNumber: must be a number"
-- pPrint $ openAroundOneCellIfMatchNumber (updateCell testField2 (Cell Flag (Position 1 0))) (cellAt testField2 (Position 1 1))

validatePos :: CellField -> Cell -> Bool
validatePos field (Cell (Number n) pos)
  = length flagCells + length fieldCells >= n
  where
    _neighbors = neighbors field pos
    flagCells  = filter isFlag  _neighbors
    fieldCells = filter isField _neighbors

data Result
  = InvalidField CellField Position
  | NoChange CellField
  | UpdatedField CellField
  deriving Show

isUpdatedField (UpdatedField _) = True
isUpdatedField _ = False

openAroundCellIfMatchNumber :: CellField -> Cell -> Result
openAroundCellIfMatchNumber field cell@(Cell (Number n) pos)
  | not $ validatePos field cell = InvalidField field pos
  | otherwise
  = case openAroundOneCellIfMatchNumber field cell of
      Nothing -> NoChange field
      Just field'
        -> foldl
           visitNeighbor
           (UpdatedField field')
           (filter isNumber $ neighbors field' pos)
  where
    visitNeighbor r@(InvalidField _ _) _ = r
    visitNeighbor (UpdatedField field) neighbor
      = proceedUpdated field neighbor
    visitNeighbor (NoChange field) neighbor
      = proceedUpdated field neighbor
    proceedUpdated field neighbor
      = case openAroundCellIfMatchNumber field neighbor of
          NoChange field -> UpdatedField field
          UpdatedField field -> UpdatedField  field
          r@(InvalidField _ _) -> r
-- pPrint $ openAroundCellIfMatchNumber (updateCell testField2 (Cell Flag (Position 0 0))) (cellAt testField2 (Position 0 1))

openAroundOneCellIfAllFlagOptionsMatchNumber field cell@(Cell (Number n) pos)
  = map Field.pos
  . filter isOpenUnknown
  . join
  . map (map head)
  . filter ((==1) . length)
  . map group
  . transpose
  . map (\(UpdatedField field) -> toList field)
--  . map (\(UpdatedField field) -> neighbors field pos)
  . filter isUpdatedField
  . map (\field -> openAroundCellIfMatchNumber field cell)
  . map (updateCells field)
  $ flagableFieldCombinations field pos

openAroundCellsIfAllFlagOptionsMatchNumber :: CellField -> [Position]
openAroundCellsIfAllFlagOptionsMatchNumber field
  = nub
  . fold
  . fmap (openAroundOneCellIfAllFlagOptionsMatchNumber field)
  . filter isNumber
  . toList
  $ field

testFieldTypes3
  = mkField
  [ [Field   , Field   , Field   , Field   , Field   ]
  , [Number 1, Number 1, Number 1, Number 1, Number 1]
  , [Open    , Open    , Open    , Open    , Open    ]
  ]
testField3 = Cell <$> testFieldTypes3 <*> positionsField 5 3

testFieldTypes4
  = mkField
  [ [Field   , Number 1, Open    ]
  , [Field   , Number 2, Number 1]
  , [Field   , Flag    , Number 1]
  , [Field   , Flag    , Number 1]
  ]
testField4 = Cell <$> testFieldTypes4 <*> positionsField 3 4

testFieldTypes5
  = mkField
  [ [Field, Field   , Field   , Field   , Field]
  , [Field, Number 1, Number 1, Number 2, Flag ]
  , [Field, Number 2, Open    , Number 2, Field]
  , [Flag , Number 2, Number 1, Number 1, Field]
  , [Field, Field   , Field   , Field   , Field]
  ]
testField5 = Cell <$> testFieldTypes5 <*> positionsField 5 5
-- openAroundCellsIfAllFlagOptionsMatchNumber testField5

openRemainingFields :: Game -> [Position]
openRemainingFields game
  | game^.flagsLeft == 0 = map pos . filter isField . toList $ game^.field
  | otherwise = []
