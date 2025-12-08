module Printing_Department where

import Data.List
import Distribution.Simple.Test.Log (countTestResults)
import GHC.Exts.Heap (ClosureType (CONSTR_0_1))
import Text.XHtml (col)

foo :: [String] =
  [ "..@@.@@@@."
  , "@@@.@.@.@@"
  , "@@@@@.@.@@"
  , "@.@@@@..@."
  , "@@.@@@@.@@"
  , ".@@@@@@@.@"
  , ".@.@.@.@@@"
  , "@.@@@.@@@@"
  , ".@@@@@@@@."
  , "@.@.@@@.@."
  ]

-- >>> parseInput foo
-- [[False,False,True,True,False,True,True,True,True,False],
--  [True,True,True,False,True,False,True,False,True,True],
--  [True,True,True,True,True,False,True,False,True,True],[True,False,True,True,True,True,False,False,True,False],
--  [True,True,False,True,True,True,True,False,True,True],
--  [False,True,True,True,True,True,True,True,False,True],
--  [False,True,False,True,False,True,False,True,True,True],
--  [True,False,True,True,True,False,True,True,True,True],
--  [False,True,True,True,True,True,True,True,True,False],
--  [True,False,True,False,True,True,True,False,True,False]]
parseInput :: [String] -> [[Bool]]
parseInput = map (map (== '@'))

-- >>> submatrix3x3 (0,7 ) (parseInput foo)
-- [[True,True],[True,False,True]]
-- >>> submatrix3x3 (0,1) (parseInput foo)
submatrix3x3 :: (Int, Int) -> [[Bool]] -> [[Bool]]
submatrix3x3 (column, row) m =
  [ [ m !! i !! j
    | j <- [row - 1 .. row + 1]
    , j >= 0
    , j < length (m !! i)
    , (i, j) /= (column, row)
    ]
  | i <- [column - 1 .. column + 1]
  , i >= 0
  , i < length m
  ]

safeSubmatrix3x3 (column, row) m =
  if column < 0 || column >= length m || row < 0 || row >= length (m !! column)
    then []
    else submatrix3x3 (column, row) m

-- >>> countAdjRolls [[True,True],[True,False,True]]
-- 4
countAdjRolls :: [[Bool]] -> Int
countAdjRolls = sum . map (length . filter id)

-- >>> canForkLiftAccess (parseInput foo) (5,0)
-- True
canForkLiftAccess :: [[Bool]] -> (Int, Int) -> Bool
canForkLiftAccess m (row, column) =
  row >= 0
    && row < length m
    && column >= 0
    && column < length (m !! row)
    && m !! row !! column
    && countAdjRolls (submatrix3x3 (row, column) m) < 4

canForkLiftMatrix :: [[Bool]] -> [[Bool]]
canForkLiftMatrix m =
  [ [canForkLiftAccess m (i, j) | j <- [0 .. length row - 1]]
  | (i, row) <- zip [0 ..] m
  ]

-- >>> shouldBePicked (parseInput foo) (0,3)
-- False
shouldBePicked :: [[Bool]] -> (Int, Int) -> Bool
shouldBePicked m (row, column) =
  row >= 0
    && row < length m
    && column >= 0
    && column < length (m !! row)
    && m !! row !! column
    && countAdjRolls (submatrix3x3 (row, column) m) >= 4

apply2D :: (a -> b) -> [[a]] -> [[b]]
apply2D f = map (map f)

pickedMatrix :: [[Bool]] -> [[Bool]]
pickedMatrix old =
  let new =
        [ [ shouldBePicked old (row, col)
          | col <- [0 .. length (old !! row) - 1]
          ]
        | row <- [0 .. length old - 1]
        ]
   in if new == old
        then old
        else pickedMatrix new

-- >>> countTrue $ parseInput foo
-- 71
countTrue :: [[Bool]] -> Int
countTrue = sum . map (sum . map fromEnum)

main = do
  contents <- readFile "Day_4/input.txt"
  let ls = lines contents
  let parsed = parseInput ls
  -- let flattened = concat $ canForkLiftMatrix parsed
  -- let totalTrues = length $ filter id flattened
  print $ countTrue parsed
  print $ countTrue $ pickedMatrix parsed
