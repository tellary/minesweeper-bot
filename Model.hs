{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Model where

import Control.Applicative
import Control.DeepSeq
import Data.Functor.Classes (Show1)
import Data.Functor.Compose
import GHC.Generics         (Generic)

data Cell
  = Cell { cellType :: CellType, pos :: Position }
  deriving (Eq, Show, Generic)
data CellType
  = Open | Number Int | Field | Flag | Mine
  deriving (Eq, Show, Generic)

instance NFData CellType
instance NFData Cell

isFlag (Cell Flag _) = True
isFlag _ = False

isField (Cell Field _) = True
isField _ = False

data Position
  = Position { x :: Int , y :: Int }
  deriving (Eq, Show, Generic)

instance NFData Position

newtype Field a
  = MkField (Compose ZipList ZipList a)
  deriving
    ( Show
    , Functor
    , Applicative
    , Foldable
    , Generic
    )

deriving instance Show1 (ZipList)
instance NFData a => NFData (Field a)

mkField :: [[a]] -> Field a
mkField f = MkField . Compose . ZipList . map ZipList $ f

unField :: Field a -> [[a]]
unField (MkField compose) = getZipList . fmap getZipList . getCompose $ compose

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
  } deriving Show
