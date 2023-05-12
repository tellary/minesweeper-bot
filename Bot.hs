{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot where

import           Control.Lens        hiding (element)
import           Control.Monad       (join)
import           Control.Monad.State
import           Data.Foldable       (fold, toList)
import           Data.List           (find, group, nub, transpose)
import           Data.Tuple          (swap)
import           Debug.Trace         (trace)
import           Field               (Cell (..), CellField, CellType (..),
                                      FieldSize (..), Position (..), cellAt,
                                      fieldAt, fieldSize, isField, isFlag,
                                      isNewFlag, isNumber, isOpen,
                                      isOpenUnknown, mkField, toNewFlag,
                                      toOpenUnknown)
import qualified Field
import           Game
import           Text.Printf         (printf)

neighborPositions pos =
  [ Position x' y'
  | dx <- [-1..1], dy <- [-1..1]
  , (dx, dy) /= (0, 0)
  , let x' = x pos + dx, let y' = y pos + dy
  ]
isNeighbor pos pos' = pos' `elem` neighborPositions pos
-- isNeighbor (Position 1 1) (Position 1 0) == True
-- isNeighbor (Position 1 1) (Position 1 1) == False
-- isNeighbor (Position 1 1) (Position 2 1) == True
-- isNeighbor (Position 1 1) (Position 2 3) == False
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
combinations _ 0 = []
combinations 0 _ = []
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

flagableNeighborCombinations :: CellField -> Position -> [[Cell]]
flagableNeighborCombinations field pos
  = case cellAt field pos of
      (Cell (Number n) _)
        -> map (map (\cell -> cell { cellType = NewFlag }))
           . filter (null . find (not . isField))
           $ _neighborCombinations
        where
          _neighbors = neighbors field pos
          _flagCells  = length $ filter isFlag _neighbors
          _combinations = combinations (n - _flagCells) (length _neighbors)
          -- neighbors choosen in each combination
          _neighborCombinations
            = map (map fst . filter snd . zip _neighbors) _combinations
      _ -> []


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
-- pPrintNoColor $ flagableNeighborCombinations testField1 (Position 1 1)
testFieldTypes1_1
  = mkField
  [ [Flag    , Open    , Flag    ]
  , [Number 1, Number 2, Number 1]
  , [Field   , Open    , Open    ]
  ]
testField1_1 = Cell <$> testFieldTypes1_1 <*> positionsField 3 3
testFieldTypes1_2
  = mkField
  [ [Open    , Open    , Flag    ]
  , [Number 1, Number 1, Number 2]
  , [Number 2, Open    , Number 2]
  ]
testField1_2 = Cell <$> testFieldTypes1_2 <*> positionsField 3 3  
-- pPrintNoColor $ flagableNeighborCombinations testField1_2 (Position 1 1)

overrideCells = overrideCellsIf (const True)
overrideCellsIf p override cells = do
  cell <- cells
  let overrideMaybe = find (\cell' -> pos cell == pos cell') override
  return $ maybe cell (\cell' -> if p cell' then cell' else cell) overrideMaybe
-- pPrint $ overrideCells (flagableNeighborCombinations testField1 (Position 1 1) !! 1) (neighbors testField1 (Position 2 1))

areFlagsPossibleOnCell field flags (Cell (Number n) pos)
  = _newNumberOfFlags <= n
  where
    _neighbors = neighbors field pos
    _overrideCells = overrideCells flags _neighbors
    _newNumberOfFlags = length . filter isFlag $ _overrideCells
areFlagsPossibleOnCell _ _ _ = error "areFlagsPossibleOnCell: must be number"
-- areFlagsPossibleOnCell testField1 (flagableNeighborCombinations testField1 (Position 1 1) !! 1) (cellAt testField1 (Position 2 1))
-- areFlagsPossibleOnCell testField1 (flagableNeighborCombinations testField1 (Position 1 1) !! 5) (cellAt testField1 (Position 2 1))

areFlagsPossibleOnAllNeighbors field pos flags
  = all _flagsPossibleOnNeighbor _numbers
  where
    _neighbors = neighbors field pos
    _numbers = filter isNumber _neighbors
    _flagsPossibleOnNeighbor = areFlagsPossibleOnCell field flags
-- pPrint $ filter (areFlagsPossibleOnAllNeighbors testField1 (Position 1 1)) $ flagableNeighborCombinations testField1 (Position 1 1)

markPosIfPossibleInAllCombinations field pos
  = join
    . map (map head)
    . filter ((==1) . length)
    . map group
    . transpose
    . map (map Field.pos)
    . filter (areFlagsPossibleOnAllNeighbors field pos)
    $ flagableNeighborCombinations field pos

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

data Action = PerformMark Position | PerformOpen Position

isPerformMark (PerformMark _) = True
isPerformMark _ = False

isPerformOpen (PerformOpen _) = True
isPerformOpen _ = False

position (PerformMark ps) = ps
position (PerformOpen ps) = ps

makeTurn :: GM [Action]
makeTurn = do
  modify
    (\game ->
       if (length . filter (not . isField) . toList $ game^.field) == 0
       then game & flagsLeft .~ initialFlags (game^.size)
       else game
    )
  toMark <- state mark
  modify (over field (flip Field.updateCells (map (Cell Flag) toMark)))
  toOpen1 <- gets (openAroundCellsIfAllFlagOptionsMatchNumber . view field)
  toOpen2 <- gets openRemainingFields
  modify
    (\game ->
       over
       flagsLeft
       (\left -> if left <= 0 then initialFlags (game^.size) else left)
       game
    )
  return $ (map PerformMark toMark) ++ map PerformOpen (nub (toOpen1 ++ toOpen2))

testFieldTypes2
  = mkField
  [ [Field   , Field   , Field   , Field   ]
  , [Number 1, Number 1, Number 1, Number 1]
  , [Open    , Open    , Open    , Open    ]
  ]
testField2 = Cell <$> testFieldTypes2 <*> positionsField 4 3
-- pPrint $ flagableNeighborCombinations testField2 (Position 1 1)
-- pPrint $ overrideCells (flagableNeighborCombinations testField2 (Position 1 1) !! 1) (neighbors testField2 (Position 2 1))

-- | Opens all fields around a cell if number of flags and fields is equal to
-- the cell's own number
openAroundOneCellIfMatchNumber :: CellField -> Cell -> Maybe CellField
openAroundOneCellIfMatchNumber field (Cell (Number n) pos)
  | length flagCells == n && length fieldCells > 0
  = Just $ foldl
    (\field fieldCell
      -> Field.updateCell field fieldCell { cellType = OpenUnknown }
    )
    field
    fieldCells
  | otherwise = Nothing
  where
    _neighbors = neighbors field pos
    flagCells  = filter isFlag  _neighbors
    fieldCells = filter isField _neighbors
openAroundOneCellIfMatchNumber _ _
  = error "openAroundOneCellIfMatchNumber: must be a number"
-- pPrint $ openAroundOneCellIfMatchNumber (updateCell testField2 (Cell Flag (Position 1 0))) (cellAt testField2 (Position 1 1))

validatePos :: CellField -> Cell -> Bool
validatePos field (Cell (Number n) pos)
  = length fieldCells >= n - length flagCells && length flagCells <= n
  where
    _neighbors = neighbors field pos
    flagCells  = filter isFlag  _neighbors
    fieldCells = filter isField _neighbors

data Result
  = InvalidField CellField Position
  | NoChange CellField
  | UpdatedField CellField
  deriving Show

isNoChangeField (NoChange _) = True
isNoChangeField _ = False

isUpdatedField (UpdatedField _) = True
isUpdatedField _ = False

isInvalidField (InvalidField _ _) = True
isInvalidField _ = False

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
  . filter isUpdatedField
  . map (\field -> openAroundCellIfMatchNumber field cell)
  . map (Field.updateCells field)
  $ flagableNeighborCombinations field pos

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

-- | Builds a new `Game` for each flag combination with cell's neighbors
-- opened and marked.
flagableNeighborCombinationsGames :: Cell -> GM [Game]
flagableNeighborCombinationsGames cell = do
  field0 <- use Game.field
  let flagCombinations = flagableNeighborCombinations field0 (pos cell)
  -- trace (printf "cell: %s, flagsCombinations: %s" (show cell) (show flagCombinations)) $ 
  forM flagCombinations $ \flags -> do
    game <- get
    return . flip execGame game $ do
      updateCells flags
      modify . over Game.field $ \field ->
        case openAroundOneCellIfMatchNumber field cell of
          Just field' -> field'
          Nothing -> field
            -- -> error
            --    (printf
            --     ( "flagableNeighborCombinations: "
            --       ++ "no cells open on cell: %s, "
            --       ++ "flags: %s, neighbors: %s"
            --     )
            --     (show cell)
            --     (show flags)
            --     (show $ neighbors field (Field.pos cell)))
-- let f = testField3 in flip evalGame (initialGame Easy f) (flagableNeighborCombinationsGames . cellAt f $ Position 1 1)

solveCell :: Int -> [Cell] -> Cell -> GM Result
solveCell 0 visited _ = do
  field <- use field
  return $ NoChange field
solveCell depth visited cell@(Cell (Number _) pos)
  | cell `elem` visited = use field >>= return . NoChange
  | otherwise = do
      field <- use field
      if (not $ validatePos field cell)
        then return $ InvalidField field pos
        else do
          cellsOpened <- case openAroundOneCellIfMatchNumber field cell of
              Just field' -> modify (set Game.field field') >> return True
              Nothing -> return False
          flagableNeighborCombinationsGames cell >>= \case
            [] -> if cellsOpened
                  then return $ UpdatedField field
                  else return $ NoChange field
            combinationGames  -> do
              let allNeighborGames = concat . flip map combinationGames
                    $ \game ->
                        let neighborNumbers
                              = filter isNumber
                              $ neighbors (view Game.field game) pos
                            neighborGames
                              = map
                                ( \neighbor ->
                                    evalGame
                                    ( solveCell
                                      (depth - 1) (cell:visited) neighbor
                                    )
                                    game
                                )
                                neighborNumbers
                        in if null $ find isInvalidField neighborGames
                           then neighborGames
                           else []
              if null allNeighborGames
                then return $ InvalidField field pos
                else
                case allNeighborGames of
                  [] -> return $ NoChange field
                  updatedGames -> do
                    modify . over Game.field
                      $ \field -> mergeNeighbors pos field
                                  $ map
                                  (\case
                                      UpdatedField field -> field
                                      NoChange field -> field
                                  )
                                  updatedGames
                    UpdatedField . view Game.field <$> get
solveCell _ _ _ = do
  field <- use field
  return $ NoChange field
-- let f = testField3 in flip evalGame (initialGame Easy f) (solveCell 5 [] . cellAt f $ Position 1 1)

mergeNeighbors :: Position -> CellField -> [CellField] -> CellField
mergeNeighbors pos baseField fields
  = Field.updateCells baseField sameOpenOrFlag
  where
    sameOpenOrFlag = filter (\cell -> isOpen cell || isFlag cell)
                   . join
                   . map (map head)
                   . filter ((==1) . length)
                   . map group
                   . map (toNewFlag)
                   . map (toOpenUnknown)
--                   . filter (isNeighbor pos . Field.pos . head)
                   . transpose
                   . map toList
                   $ fields

fromUpdatedField (UpdatedField field) = field

solveDepth = 10

solve :: GM ([Position], [Position])
solve = do
  mapM_ (solveCell solveDepth []) . toList . view Game.field =<< get
  openCells <- gets (filter isOpenUnknown . toList . view Game.field)
  let open = map Field.pos $ openCells
  markCells <- gets (filter isNewFlag . toList . view Game.field)
  let mark = map Field.pos $ markCells
  updateCells markCells
  updateCells openCells
  return (mark, open)
-- let f = testField3 in flip evalGame (initialGame Easy f) solve

solve2 = do
  solve
  solve
--  solve

testFieldTypes6
  = mkField
  [ [Field   , Field   , Field   ]
  , [Number 1, Number 3, Flag    ]
  , [Open    , Open    , Open    ]
  ]
testField6 = Cell <$> testFieldTypes6 <*> positionsField 3 3

testFieldTypes7
  = mkField
  [ [ Flag, Flag    , Field   , Flag    , Flag ]
  , [ Flag, Number 5, Number 4, Number 5, Flag ]
  , [ Flag, Field   , Field   , Field   , Flag ]
  ]
testField7 = Cell <$> testFieldTypes7 <*> positionsField 5 3

testFieldTypes8
  = mkField
  [ [Field, Field   , Field   , Field   , Field   ]
  , [Field, Number 1, Number 2, Number 3, Flag    ]
  , [Field, Open    , Open    , Number 1, Number 1]
  ]
testField8 = Cell <$> testFieldTypes8 <*> positionsField 5 3

testFieldTypes9
  = mkField
  [ [Field   , Field   , Flag    , Flag    ]
  , [Number 2, Flag    , Number 3, Number 2]
  , [Flag    , Number 4, Number 3, Number 1]
  , [Flag    , Field   , Field   , Field   ]
  ]
testField9 = Cell <$> testFieldTypes9 <*> positionsField 4 4
