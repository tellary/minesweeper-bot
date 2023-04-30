{-# LANGUAGE RecordWildCards   #-}

module Bot where

import Data.Foldable (fold, toList)
import Data.List     (find, nub)
import Model

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
          -> map Model.pos fieldCells
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
  = nub . fold . fmap (markPosIfMatchNumber field . pos) $ field

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

flagableFieldCombinations field pos n
  = filter (null . find (not . isField))
  $ _neighborCombinations
  where
    _neighbors = neighbors field pos
    _flagCells  = length $ filter isFlag _neighbors
    _combinations = combinations (n - _flagCells) (length _neighbors)
    _neighborCombinations
      = map (map fst . filter snd . zip _neighbors) _combinations

positionsField n m
  = mkField
  [ [ Position x y | x <- [0 .. n - 1] ]
    | y <- [0 .. m - 1]
  ]
testFieldTypes1
  = mkField
  [ [Field, Open    , Field]
  , [Open , Number 2, Open ]
  , [Field, Open    , Open ]
  ]
testField1 = Cell <$> testFieldTypes1 <*> positionsField 3 3
-- pPrintNoColor $ flagableFieldCombinations testField1 (Position 1 1) 2
