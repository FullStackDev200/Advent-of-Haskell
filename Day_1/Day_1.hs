module Secret_Entrance where

import Control.Exception (handle)
import Data.ByteString (StrictByteString)
import Data.Char
import System.IO

foo =
  [ "L68"
  , "L30"
  , "R48"
  , "L5"
  , "R60"
  , "L55"
  , "L1"
  , "L99"
  , "R14"
  , "L82"
  ]

-- >>> solution' "R48" 52
-- 0
solution' :: String -> Integer -> Integer
solution' ('R' : xs) start
  | start + n >= 100 = (start + n - 100) `mod` 100
  | otherwise = start + n
 where
  n = read xs :: Integer
solution' ('L' : xs) start
  | start - n <= 0 = (start - n + 100) `mod` 100
  | otherwise = start - n
 where
  n = read xs :: Integer

-- >>> solutionList foo 50
-- [82,52,0,95,55,0,99,0,14,32]
solutionList :: [String] -> Integer -> [Integer]
solutionList [] _start = []
solutionList (x : xs) start =
  let result = solution' x start
   in result : solutionList xs result

-- >>> main
main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let vlist = solutionList ls 50
  let solution = filter (== 0) vlist
  print (length solution)
