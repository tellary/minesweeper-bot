{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens hiding (element)
import Control.Lens.TH
import Field

data GameSize = Easy | Medium | Hard deriving Show

data Game = Game { _size :: GameSize, _flagsLeft :: Int, _field :: CellField } deriving Show

$(makeLenses ''Game)

initialFlags Easy = 10
initialFlags Hard = 99
