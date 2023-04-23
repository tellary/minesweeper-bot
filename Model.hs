module Model where

data Cell = Open | Number Int | Field | Flag | Mine deriving (Eq, Show)

data Position = Position Int Int

type Field = [[Cell]]

data FieldSize
  = FieldSize
  { fieldWidth :: Int
  , fieldHeight :: Int
  , cellSize :: Int
  } deriving Show
