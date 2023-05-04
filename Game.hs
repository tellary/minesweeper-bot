{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens        hiding (element)
import Control.Lens.TH
import Control.Monad.State
import Field

data GameSize = Easy | Medium | Hard deriving Show

data Game = Game { _size :: GameSize, _flagsLeft :: Int, _field :: CellField } deriving Show

$(makeLenses ''Game)

initialFlags Easy = 10
initialFlags Hard = 99

newtype GM a = GM (State Game a) deriving (Functor, Applicative, Monad, MonadState Game)

runGame :: GM a -> Game -> (a, Game)
runGame (GM st) game = runState st game
