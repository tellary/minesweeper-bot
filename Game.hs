{-# LANGUAGE TemplateHaskell #-}

module Game where

import           Control.Lens        hiding (element)
import           Control.Lens.TH
import           Control.Monad.State
import           Field               (CellField)
import qualified Field

data GameSize = Easy | Medium | Hard deriving (Show, Eq)

data Game
  = Game
  { _size :: GameSize
  , _previous :: Maybe CellField
  , _flagsLeft :: Int
  , _field :: CellField
  } deriving (Show, Eq)

$(makeLenses ''Game)

initialFlags Easy = 10
initialFlags Hard = 99

initialGame size = Game size Nothing (initialFlags size)

newtype GM a = GM (State Game a) deriving (Functor, Applicative, Monad, MonadState Game)

runGame :: GM a -> Game -> (a, Game)
runGame (GM st) game = runState st game

evalGame :: GM a -> Game -> a
evalGame (GM st) game = evalState st game

execGame :: GM a -> Game -> Game
execGame (GM st) game = execState st game

updateCells :: Foldable t => t Field.Cell -> GM ()
updateCells cells = do
  field0 <- use field
  let (field1, newFlags) = Field.updateCells field0 cells
  modify $ \game ->
    set flagsLeft ((view flagsLeft game) - newFlags)
    . set field field1
    $ game
