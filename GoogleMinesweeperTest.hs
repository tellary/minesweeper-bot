module GoogleMinesweeperTest where

import Bot
import Field
import Game

positionsField n m
  = mkField
  [ [ Position x y | x <- [0 .. n - 1] ]
    | y <- [0 .. m - 1]
  ]
testFieldTypes1
  = mkField
  [ [Field   , Field   , Field   ]
  , [Number 1, Number 2, Number 1]
  , [Field   , Open    , Open    ]
  ]
testField1 = Cell <$> testFieldTypes1 <*> positionsField 3 3
-- pPrintNoColor $ flagableNeighborCombinations testField1 (Position 1 1)
testFieldTypes1_1
  = mkField
  [ [Flag    , Open    , Flag    ]
  , [Number 1, Number 2, Number 1]
  , [Field   , Open    , Open    ]
  ]
testField1_1 = Cell <$> testFieldTypes1_1 <*> positionsField 3 3
testFieldTypes1_2
  = mkField
  [ [Open    , Open    , Flag    ]
  , [Number 1, Number 1, Number 2]
  , [Number 2, Open    , Number 2]
  ]
testField1_2 = Cell <$> testFieldTypes1_2 <*> positionsField 3 3  
-- pPrintNoColor $ flagableNeighborCombinations testField1_2 (Position 1 1)

testFieldTypes2
  = mkField
  [ [Field   , Field   , Field   , Field   ]
  , [Number 1, Number 1, Number 1, Number 1]
  , [Open    , Open    , Open    , Open    ]
  ]
testField2 = Cell <$> testFieldTypes2 <*> positionsField 4 3
-- pPrint $ flagableNeighborCombinations testField2 (Position 1 1)
-- pPrint $ overrideCells (flagableNeighborCombinations testField2 (Position 1 1) !! 1) (neighbors testField2 (Position 2 1))

testFieldTypes3
  = mkField
  [ [Field   , Field   , Field   , Field   , Field   ]
  , [Number 1, Number 1, Number 1, Number 1, Number 1]
  , [Open    , Open    , Open    , Open    , Open    ]
  ]
testField3 = Cell <$> testFieldTypes3 <*> positionsField 5 3

testFieldTypes4
  = mkField
  [ [Field   , Number 1, Open    ]
  , [Field   , Number 2, Number 1]
  , [Field   , Flag    , Number 1]
  , [Field   , Flag    , Number 1]
  ]
testField4 = Cell <$> testFieldTypes4 <*> positionsField 3 4

testFieldTypes5
  = mkField
  [ [Field, Field   , Field   , Field   , Field]
  , [Field, Number 1, Number 1, Number 2, Flag ]
  , [Field, Number 2, Open    , Number 2, Field]
  , [Flag , Number 2, Number 1, Number 1, Field]
  , [Field, Field   , Field   , Field   , Field]
  ]
testField5 = Cell <$> testFieldTypes5 <*> positionsField 5 5

testFieldTypes6
  = mkField
  [ [Field   , Field   , Field   ]
  , [Number 1, Number 3, Flag    ]
  , [Open    , Open    , Open    ]
  ]
testField6 = Cell <$> testFieldTypes6 <*> positionsField 3 3

testFieldTypes7
  = mkField
  [ [ Flag, Flag    , Field   , Flag    , Flag ]
  , [ Flag, Number 5, Number 4, Number 5, Flag ]
  , [ Flag, Field   , Field   , Field   , Flag ]
  ]
testField7 = Cell <$> testFieldTypes7 <*> positionsField 5 3

testFieldTypes8
  = mkField
  [ [Field, Field   , Field   , Field   , Field   ]
  , [Field, Number 1, Number 2, Number 3, Flag    ]
  , [Field, Open    , Open    , Number 1, Number 1]
  ]
testField8 = Cell <$> testFieldTypes8 <*> positionsField 5 3

testFieldTypes9
  = mkField
  [ [Field   , Field   , Flag    , Flag    ]
  , [Number 2, Flag    , Number 3, Number 2]
  , [Flag    , Number 4, Number 3, Number 1]
  , [Flag    , Field   , Field   , Field   ]
  ]
testField9 = Cell <$> testFieldTypes9 <*> positionsField 4 4

solve9 =
  let f = testField9 in flip evalGame (initialGame Easy f) solve
testSolve9
  = solve9
  ==
  ( True
  , [ Position {x = 1, y = 3}
    , Position {x = 3, y = 3}
    ]
  , [ Position {x = 0, y = 0}
    , Position {x = 1, y = 0}
    , Position {x = 2, y = 3}
    ]
  )

solveCell9_2_3 =
  let f = testField9
  in flip evalGame (initialGame Easy f) (solveCell 10 [] (Cell (Number 4) (Position 1 2)))

testSolveCell9_2_3
  = let UpdatedField field = solveCell9_2_3
    in cellType (cellAt field (Position 1 3)) == NewFlag
       && cellType (cellAt field (Position 2 3)) == OpenUnknown
       
