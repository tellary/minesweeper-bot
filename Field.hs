{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Field where

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Data.Functor.Classes (Eq1, Show1)
import Data.Functor.Compose
import Data.List            (find)
import GHC.Generics         (Generic)

data Cell
  = Cell { cellType :: CellType, pos :: Position }
  deriving (Eq, Show, Ord, Generic)
data CellType
  = Open | OpenUnknown | Number Int | Field | Flag | NewFlag | Mine 
  deriving (Eq, Show, Ord, Generic)

instance NFData CellType
instance NFData Cell

isFlag (Cell Flag _) = True
isFlag (Cell NewFlag _) = True
isFlag _ = False

isNewFlag (Cell NewFlag _) = True
isNewFlag _ = False

isField (Cell Field _) = True
isField _ = False

isNumber (Cell (Number _) _) = True
isNumber _ = False

isOpen (Cell Open _) = True
isOpen (Cell OpenUnknown _) = True
isOpen _ = False

isOpenUnknown (Cell OpenUnknown _) = True
isOpenUnknown _ = False

toOpenUnknown cells
  | all isOpen cells && (not . null $ find isOpenUnknown cells)
  = map (\cell -> cell { cellType = OpenUnknown }) cells
  | otherwise = cells

toNewFlag cells
  | all isFlag cells && (not . null $ find isNewFlag cells)
  = map (\cell -> cell { cellType = NewFlag }) cells
  | otherwise = cells

data Position
  = Position { x :: Int , y :: Int }
  deriving (Eq, Show, Ord, Generic)

instance NFData Position

newtype Field a
  = MkField (Compose ZipList ZipList a)
  deriving
    ( Show
    , Eq
    , Functor
    , Applicative
    , Foldable
    , Generic
    )

deriving instance Show1 (ZipList)
deriving instance Eq1 (ZipList)
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

updateAt field (Position x y) a
  = mkField ((unField field) & element y . element x .~ a)

updateCell field cell@(Cell _ pos)
  = updateAt field pos cell

updateCells field cells
  = foldl updateCountNewFlags (field, 0) cells
  where
    updateCountNewFlags (field, countOpen) cell
      | isFlag . cellAt field . pos $ cell = (updateCell field cell, countOpen)
      | isFlag cell = (updateCell field cell, countOpen + 1)
      | otherwise = (updateCell field cell, countOpen)

data ImgFieldSize
  = ImgFieldSize
  { imgFieldSize :: FieldSize
  , imgCellSize :: Int
  } deriving Show
