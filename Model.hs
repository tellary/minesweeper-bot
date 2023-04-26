{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where

import Data.Functor.Compose

data Cell = Cell { cellType :: CellType, pos :: Position } deriving (Eq, Show)
data CellType = Open | Number Int | Field | Flag | Mine deriving (Eq, Show)

data Position = Position { x :: Int , y :: Int } deriving (Eq, Show)

newtype Field a
  = MkField (Compose [] [] a)
  deriving (Functor, Applicative, Foldable)

mkField :: [[a]] -> Field a
mkField f = MkField $ Compose f
unField (MkField compose) = getCompose compose

type CellField = Field Cell

data FieldSize
  = FieldSize
  { fieldWidth :: Int
  , fieldHeight :: Int
  , cellSize :: Int
  } deriving Show

fieldAt :: Field a -> Position -> a
fieldAt field pos = cellRow !! (x pos)
  where
    cellRow = unField field !! (y pos)

cellAt :: CellField -> Position -> Cell
cellAt = fieldAt
