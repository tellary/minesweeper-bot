module Model where

data Cell = Cell { cellType :: CellType, pos :: Position } deriving (Eq, Show)
data CellType = Open | Number Int | Field | Flag | Mine deriving (Eq, Show)

data Position = Position { x :: Int , y :: Int } deriving (Eq, Show)

type Field = [[Cell]]

data FieldSize
  = FieldSize
  { fieldWidth :: Int
  , fieldHeight :: Int
  , cellSize :: Int
  } deriving Show

cellAt :: Field -> Position -> Cell
cellAt field pos = cellRow !! (x pos)
  where
    cellRow = field !! (y pos)
