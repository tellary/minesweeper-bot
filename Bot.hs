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

data Action = PerformMark Position | PerformOpen Position

isPerformMark (PerformMark _) = True
isPerformMark _ = False

isPerformOpen (PerformOpen _) = True
isPerformOpen _ = False

position (PerformMark ps) = ps
position (PerformOpen ps) = ps

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
            [] -> do
              field' <- use Game.field
              if cellsOpened
                then return $ UpdatedField field'
                else return $ NoChange field'
            combinationGames -> do
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
                           else [] -- This game doesn't contribute,
                                   -- because it's invalid for one of neighbors
              if null allNeighborGames
                then (\field -> InvalidField field pos) <$> use Game.field
                else do
                  let (merged, mergedNeighborsField)
                        = mergeNeighbors pos field
                          $ map
                            (\case
                                UpdatedField field -> field
                                NoChange field -> field
                            )
                            $ allNeighborGames
                  if merged
                    then do
                      modify (set Game.field mergedNeighborsField)
                      UpdatedField <$> use Game.field
                    else if cellsOpened
                         then UpdatedField <$> use Game.field
                         else NoChange <$> use Game.field
solveCell _ _ _ = do
  field <- use field
  return $ NoChange field
-- let f = testField3 in flip evalGame (initialGame Easy f) (solveCell 5 [] . cellAt f $ Position 1 1)

mergeNeighbors :: Position -> CellField -> [CellField] -> (Bool, CellField)
mergeNeighbors pos baseField fields
  = (not . null $ sameOpenOrFlag, Field.updateCells baseField sameOpenOrFlag)
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

solve :: GM (Bool, [Position], [Position])
solve = do
  fieldBefore <- use field
  cells <- cellsAffectedByUpdate <$> get
  mapM_ (solveCell solveDepth []) cells
  openCells <- gets (filter isOpenUnknown . toList . view Game.field)
  let open = map Field.pos $ openCells
  markCells <- gets (filter isNewFlag . toList . view Game.field)
  let mark = map Field.pos $ markCells
  updateCells markCells
  updateCells openCells
  modify $ set previous (Just fieldBefore)
  return (not . null $ cells, mark, open)

changedCells game = case view previous $ game of
  Nothing -> toList . view field $ game
  Just previousField
    -> map snd . filter (uncurry (/=))
       $ zip
         (toList previousField)
         (toList . view Game.field $ game)

cellsAffectedByUpdate game = case view previous $ game of
  Nothing -> toList . view field $ game
  Just _
    -> concat
       . map
         (\cell -> filter isNumber
                   . (cell :) . neighbors (view field game)
                   . Field.pos $ cell
         )
       . changedCells
       $ game
    
solve2 =
  let loop =
        do
          r@(updated, mark, open) <- solve
          if updated then loop
            else return r
  in do
    (_, mark, open) <- loop
    return (mark, open)
