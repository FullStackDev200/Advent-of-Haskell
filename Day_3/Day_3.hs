module Lobby where

import Data.Char (digitToInt)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Ord

foo =
  [ 987654321111111
  , 811111111111119
  , 234234234234278
  , 818181911112111
  ]

-- >>> digits 11
-- [1,1]
digits :: (Show a, Integral a) => a -> [Int]
digits = map (\c -> read [c] :: Int) . show

-- >>> fromDigits [1,2,3]
-- 123
fromDigits :: (Show a) => [a] -> Integer
fromDigits = read . concatMap show

-- >>> lastIndexOf 9 $ digits 9876543211911111
-- Just 10
lastIndexOf :: (Eq a) => a -> [a] -> Maybe Int
lastIndexOf x xs =
  case elemIndices x xs of
    [] -> Nothing -- element not found
    idxs -> Just (last idxs) -- take the last index

sortPair :: (Ord a) => (a, a) -> (a, a)
sortPair (x, y) =
  let [a, b] = sort [x, y]
   in (a, b)

maxElem :: (Ord a) => [a] -> Maybe a
maxElem = foldr (max . Just) Nothing

-- >>> maxJoltage 9876543211911111
maxJoltage :: Integer -> Integer
maxJoltage x
  | m == n = fromDigits [m, m]
  | fromMaybe 0 (lastIndexOf m ds) < fromMaybe 0 (lastIndexOf n ds) = fromDigits [m, n]
  | fromMaybe 0 (lastIndexOf m ds) > fromMaybe 0 (lastIndexOf k ds) = fromDigits [k, m]
  | fromMaybe 0 (lastIndexOf m ds) > fromMaybe 0 (lastIndexOf n ds) = fromDigits [m, k]
  | otherwise = error "Something went wrong"
 where
  ds = digits x
  (m : n : _) = sortBy (comparing Data.Ord.Down) (digits x)
  k = fromMaybe n (maxElem rest)
  rest = drop (fromMaybe 0 (lastIndexOf m ds) + 1) ds

-- >>> maxJoltage' (digits 5435113354324445355543423523324245133533362334234141663246333323544553332443432533433423343343453463) 0 12
-- maxJoltage' :: [Int] -> Int -> Int -> Integer
-- maxJoltage' x start solutionLength
--   | length x < start + solutionLength = 0
--   | otherwise = fromDigits [toInteger localMax, maxJoltage' x (fromMaybe 0 $ elemIndex localMax x) (solutionLength - 1)]
--  where
--   cropList xs a b = drop a (take b xs)
--   localMax = maximum (cropList x start (length x - start - solutionLength + 1))

-- >>> maxJoltage' 5435113354324445355543423523324245133533362334234141663246333323544553332443432533433423343343453463 0 12
-- 666655555463
maxJoltage' :: Integer -> Int -> Int -> Integer
maxJoltage' x start solutionLength =
  case solve (digits x) start solutionLength of
    "" -> 0
    s -> read s
 where
  solve :: [Int] -> Int -> Int -> String
  solve ds currentStart len
    | len == 0 = ""
    | length ds < currentStart + len = ""
    | otherwise =
        let searchWindow = take (length ds - currentStart - len + 1) (drop currentStart ds)
            localMax = maximum searchWindow
            Just maxIndex = elemIndex localMax (drop currentStart ds)
            nextStart = currentStart + maxIndex + 1
         in show localMax ++ solve ds nextStart (len - 1)

main = do
  contents <- readFile "Day_3/input.txt"
  let ls = lines contents
  let results = map (\x -> maxJoltage' (read x) 0 12) ls
  let finalSum = sum results
  print finalSum
