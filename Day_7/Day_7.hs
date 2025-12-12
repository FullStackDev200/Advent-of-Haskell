module Laboratories where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Tree (flatten)

foo =
  [ ".......S......."
  , "..............."
  , ".......^......."
  , "..............."
  , "......^.^......"
  , "..............."
  , ".....^.^.^....."
  , "..............."
  , "....^.^...^...."
  , "..............."
  , "...^.^...^.^..."
  , "..............."
  , "..^...^.....^.."
  , "..............."
  , ".^.^.^.^.^...^."
  , "..............."
  ]

parseChar :: Char -> Int
parseChar '.' = 0
parseChar 'S' = 1
parseChar '^' = 2

parseInput :: [String] -> [[Int]]
parseInput = map (map parseChar)

toArray :: [[Int]] -> Array (Int, Int) Int
toArray m =
  array
    ((0, 0), (rows - 1, cols - 1))
    [ ((i, j), m !! i !! j)
    | i <- [0 .. rows - 1]
    , j <- [0 .. cols - 1]
    ]
 where
  rows = length m
  cols = length (m !! 1)

fromArray :: Array (Int, Int) Int -> [[Int]]
fromArray arr =
  [ [ arr ! (i, j)
    | j <- [c0 .. c1]
    ]
  | i <- [r0 .. r1]
  ]
 where
  ((r0, c0), (r1, c1)) = bounds arr

-- 1 = beam, 2 = splitter, 0 = empty
startBeams :: Int -> Int -> Int -> Int
startBeams self before next
  | before == 2 || next == 2 = 1
  | otherwise = self

extendBeams :: Int -> Int -> Int
extendBeams self up
  | up == 1 = 1
  | otherwise = self

getOr :: Int -> [[Int]] -> Int -> Int -> Int
getOr def m i j =
  if i >= 0 && i < length m && j >= 0 && j < length (m !! i)
    then m !! i !! j
    else def

buildTree :: [[Int]] -> [[Int]]
buildTree m =
  [ [ startBeams
        (m !! i !! j)
        (getOr 0 m i (j - 1))
        (getOr 0 m i (j + 1))
    | j <- [0 .. length (m !! i) - 1]
    ]
  | i <- [0 .. length m - 1]
  ]

fixPointArr :: (Eq a) => (a -> a) -> a -> a
fixPointArr f x =
  let next = f x
   in if next == x then x else fixPointArr f next

extendTreeST :: Array (Int, Int) Int -> Array (Int, Int) Int
extendTreeST old = runSTArray $ do
  st <- thaw old
  let ((r0, c0), (r1, c1)) = bounds old

  forM_ [r0 .. r1] $ \i ->
    forM_ [c0 .. c1] $ \j -> do
      self <- readArray st (i, j)

      if self == 2
        then pure ()
        else do
          up <-
            if i > r0
              then readArray st (i - 1, j)
              else pure 0

          let new = extendBeams self up
          writeArray st (i, j) new

  pure st

solve :: [[Int]] -> [[Int]]
solve m =
  let built = buildTree m
      arr0 = toArray built
      arrF = fixPointArr extendTreeST arr0
   in fromArray arrF

isValidSplit :: Int -> Int -> Int -> Int
isValidSplit self up before
  | self == 1 && up == 1 && before /= 2 = 1
  | otherwise = 0

countSplits :: [[Int]] -> Int
countSplits m =
  sum
    [ isValidSplit (m !! i !! j) (getOr 0 m (i - 1) j)
    | i <- [0 .. length m - 1]
    , j <- [0 .. length (m !! i) - 1]
    ]
 where
  isValidSplit self up =
    if self == 2 && up == 1 then 1 else 0

render :: [[Int]] -> String
render =
  unlines . map (map f)
 where
  f 0 = '.'
  f 1 = '|'
  f 2 = '^'

main = do
  contents <- readFile "Day_7/input.txt"
  let ls = lines contents
  let input = parseInput ls
  print $ countSplits (solve input)
