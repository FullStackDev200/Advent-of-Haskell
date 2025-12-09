module Cafeteria where

import Data.List

fooRange =
  [ "3-5"
  , "10-14"
  , "16-20"
  , "12-18"
  ]

fooID =
  [ 1
  , 5
  , 8
  , 11
  , 17
  , 32
  ]

-- >>> makeRange (11,33)
-- [11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33]
makeRange :: (Integer, Integer) -> [Integer]
makeRange (x, y) = [x .. y]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
   where
    (w, s'') = break p s'

-- >>> parseRange "3-5"
-- (3,5)
parseRange :: String -> (Integer, Integer)
parseRange s =
  let parts = wordsWhen (== '-') s
   in case parts of
        [start, end] -> (read start :: Integer, read end :: Integer)
        _ -> error ("Invalid range format: " ++ s)

mergeRange :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mergeRange (a, b) (x, y)
  | b >= x = (a, max b y)
  | otherwise = error "non-overlapping ranges"

-- >>> mergeRanges $ map parseRange fooRange
-- [(3,5),(10,20)]
mergeRanges :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeRanges [] = []
mergeRanges rs =
  foldl mergeAcc [head sorted] (tail sorted)
 where
  sorted = sortBy (\(a, _) (b, _) -> compare a b) rs

  mergeAcc :: [(Integer, Integer)] -> (Integer, Integer) -> [(Integer, Integer)]
  mergeAcc acc r =
    let lastRange = last acc
     in if snd lastRange >= fst r
          then init acc ++ [mergeRange lastRange r] -- merge
          else acc ++ [r] -- no overlap

-- >>> isNumberBetweenRanges 4 (3,5)
-- True
areNumbersBetweenRanges :: Integer -> [(Integer, Integer)] -> Bool
areNumbersBetweenRanges x = any (\(a, b) -> x >= a && x <= b)

lengthOfRanges :: (Integer, Integer) -> Integer
lengthOfRanges (a, b) = b - a + 1

-- >>> sortBy (\(a,_) (b,_) -> compare a b) $ map $ parseRange fooRange

-- >>> slice 1 4 [1..10]
-- [3,4,5,6]
slice :: Int -> Int -> [a] -> [a]
slice start end xs = take (end - start) (drop start xs)

main = do
  contents <- readFile "Day_5/input.txt"
  let ls = lines contents

  let ranges = slice 0 173 ls
  print ranges

  -- let ranges = fooRange
  let numberStrings = slice 175 (length ls) ls

  let numbers = map read numberStrings :: [Integer]
  -- let numbers = fooID

  let mergedRanges = mergeRanges $ map parseRange ranges
  let result = sum $ map lengthOfRanges mergedRanges
  print result

-- let results = map (`areNumbersBetweenRanges` mergedRanges) numbers
--
-- let numTrues = length (filter id results)
--
-- print mergedRanges
-- print numbers
-- print numTrues

-- print $ ls !! 1
-- print $ length ls
