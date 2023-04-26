{-# LANGUAGE RecordWildCards   #-}

module Bot where

import Data.Foldable (fold, toList)
import Data.List     (nub)
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
