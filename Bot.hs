{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot where

import Control.Lens        hiding (element)
import Control.Monad       (join)
import Control.Monad.State
import Data.Foldable       (fold, toList)
import Data.List           (find, group, transpose)
import Data.List.Extra     (nubOrd)
import Data.Tuple          (swap)
import Debug.Trace         (trace)
import Field               (Cell (..), CellField, CellType (..), FieldSize (..),
                            Position (..), cellAt, fieldAt, fieldSize, isField,
                            isFlag, isNewFlag, isNumber, isOpen, isOpenUnknown,
                            mkField, toNewFlag, toOpenUnknown)
import Game
import Text.Printf         (printf)

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

flagableNeighborCombinations :: Position -> GM [[Cell]]
flagableNeighborCombinations pos = do
  field <- use field
  flagsLeft <- use flagsLeft
  case cellAt field pos of
    (Cell (Number n) _)
      -> return
         . map (map (\cell -> cell { cellType = NewFlag }))
         . filter (null . find (not . isField))
         $ _neighborCombinations
      where
        _neighbors = neighbors field pos
        _flagCellsNum  = length $ filter isFlag _neighbors
        _newFlagsNum = min (n - _flagCellsNum) flagsLeft
        _combinations = combinations _newFlagsNum (length _neighbors)
        -- neighbors choosen in each combination
        _neighborCombinations
          = map (map fst . filter snd . zip _neighbors) _combinations
    _ -> return []

data Action = PerformMark Position | PerformOpen Position

isPerformMark (PerformMark _) = True
isPerformMark _ = False

isPerformOpen (PerformOpen _) = True
isPerformOpen _ = False

position (PerformMark ps) = ps
position (PerformOpen ps) = ps

-- | Opens all fields around a cell if number of flags and fields is equal to
-- the cell's own number
openAroundOneCellIfMatchNumber :: Cell -> GM Bool
openAroundOneCellIfMatchNumber (Cell (Number n) pos) = use field >>= \case
  field | length flagCells == n && length fieldCells > 0 -> do
            updateCells
              . map (\cell -> cell { cellType = OpenUnknown })
              $ fieldCells
            return True
        | otherwise -> return False
    where
      _neighbors = neighbors field pos
      flagCells  = filter isFlag  _neighbors
      fieldCells = filter isField _neighbors
openAroundOneCellIfMatchNumber _
  = error "openAroundOneCellIfMatchNumber: must be a number"
-- pPrint $ openAroundOneCellIfMatchNumber (updateCell testField2 (Cell Flag (Position 1 0))) (cellAt testField2 (Position 1 1))

validatePos :: Int -> CellField -> Cell -> Bool
validatePos flagsLeft field (Cell (Number n) pos)
  | length fieldCells < n - length flagCells = False
  | length flagCells > n = False
  | flagsLeft < n - length flagCells
  = trace "validatePos: not enough flags left" $ False
  | otherwise = True
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

resultType (InvalidField _ _) = "invalid"
resultType (NoChange _) = "no change"
resultType (UpdatedField _) = "updated"

openRemainingFields :: GM ()
openRemainingFields = get >>= \case
  game | view flagsLeft game == 0 -> updateCells opened
       | otherwise -> return ()
    where opened = map (\cell -> cell { cellType = OpenUnknown } )
                   . filter isField
                   . toList
                   . view field
                   $ game

-- | Builds a new `Game` for each flag combination with cell's neighbors
-- opened and marked.
flagableNeighborCombinationsGames :: Cell -> GM [Game]
flagableNeighborCombinationsGames cell = do
  flagCombinations <- flagableNeighborCombinations (pos cell)
  forM flagCombinations $ \flags -> do
    game <- get
    return . flip execGame game $ do
      updateCells flags
      openAroundOneCellIfMatchNumber cell
      get

solveCell :: Int -> [Cell] -> Cell -> GM Result
solveCell 0 visited _ = do
  field <- use field
  return $ NoChange field
solveCell depth visited cell@(Cell (Number _) pos)
  | cell `elem` visited = use field >>= return . NoChange
  | otherwise = do
      field <- use field
      flagsLeft <- use flagsLeft
      if (not $ validatePos flagsLeft field cell)
        then return $ InvalidField field pos
        else do
          cellsOpened <- openAroundOneCellIfMatchNumber cell
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
                            -- Validate immediate neighbors to avoid considering
                            -- neighbors of an immediate neighbor when some
                            -- other immediate neighbor is invalid.
                            allValid
                              = all
                                ( validatePos
                                  (view Game.flagsLeft game)
                                  (view Game.field game)
                                )
                                neighborNumbers
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
                        in if allValid
                              then if null $ find isInvalidField neighborGames
                                   then neighborGames
                                   else []
                                        -- This game doesn't contribute,
                                        -- because it's invalid for one of neighbors
                           else trace "Invalid neighbor" []
              if null allNeighborGames
                then (\field -> InvalidField field pos) <$> use Game.field
                else do
                  merged <- mergeNeighbors pos
                            $ map
                            (\case
                                UpdatedField field -> field
                                NoChange field -> field
                            )
                            $ allNeighborGames
                  if merged
                    then UpdatedField <$> use Game.field
                    else if cellsOpened
                         then UpdatedField <$> use Game.field
                         else NoChange <$> use Game.field
solveCell _ _ _ = do
  field <- use field
  return $ NoChange field
-- let f = testField3 in flip evalGame (initialGame Easy f) (solveCell 5 [] . cellAt f $ Position 1 1)

mergeNeighbors :: Position -> [CellField] -> GM Bool
mergeNeighbors pos fields = do
  updateCells sameOpenOrFlag
  return . not . null $ sameOpenOrFlag
  where
    sameOpenOrFlag = filter (\cell -> isOpen cell || isFlag cell)
                   . join
                   . map (map head)
                   . filter ((==1) . length)
                   . map group
                   . map (toNewFlag)
                   . map (toOpenUnknown)
                   . filter (isNeighbor pos . Field.pos . head)
                   . transpose
                   . map toList
                   $ fields

solveDepth = 5000

solve :: GM (Bool, [Position], [Position])
solve = do
  fieldBefore <- use field
  let loop _        [] = return ()
      loop visited (cell:cells) = do
        result <- solveCell solveDepth [] cell
        if isUpdatedField result
          then do
            field' <- use field
            let cells' = filter (not . (`elem` visited))
                         . filter isNumber
                         . neighbors field'
                         . pos $ cell
            loop (cell:visited) (nubOrd $ cells' ++ cells)
          else loop visited cells
  cells <- cellsAffectedByUpdate <$> get
  loop [] cells
  openRemainingFields
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
