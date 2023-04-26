{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where

import Data.Functor.Compose

data Cell = Cell { cellType :: CellType, pos :: Position } deriving (Eq, Show)
data CellType = Open | Number Int | Field | Flag | Mine deriving (Eq, Show)

isFlag (Cell Flag _) = True
isFlag _ = False

isField (Cell Field _) = True
isField _ = False

data Position = Position { x :: Int , y :: Int } deriving (Eq, Show)

newtype Field a
  = MkField (Compose [] [] a)
  deriving (Show, Functor, Applicative, Foldable)

mkField :: [[a]] -> Field a
mkField f = MkField $ Compose f
unField (MkField compose) = getCompose compose

type CellField = Field Cell

data FieldSize
  = FieldSize
  { fieldWidth :: Int
  , fieldHeight :: Int
  } deriving Show

fieldSize field
  | length field == 0
  = FieldSize
    { fieldWidth  = 0
    , fieldHeight = 0
    }
  | otherwise
  = FieldSize
    { fieldWidth  = length $ head _field
    , fieldHeight = length _field
    }
  where
    _field = unField field

fieldAt :: Field a -> Position -> a
fieldAt field pos = cellRow !! (x pos)
  where
    cellRow = unField field !! (y pos)

cellAt :: CellField -> Position -> Cell
cellAt = fieldAt

data ImgFieldSize
  = ImgFieldSize
  { imgFieldSize :: FieldSize
  , imgCellSize :: Int
  }
