module Trash_Compactor where

import Data.Char (isDigit, isSpace)
import Data.List

parseCell :: String -> (String, String)
parseCell s =
  let leading = takeWhile isSpace s
      rest = dropWhile isSpace s
      digits = takeWhile isDigit rest
      after = dropWhile isDigit rest
   in (leading ++ digits, after)

parseLine :: String -> [String]
parseLine "" = []
parseLine s =
  let (cell, rest) = parseCell s
      rest' = dropWhile (== ' ') rest
   in cell : parseLine rest'

-- >>> parseMatrix foo
-- [["123","328","51","64"],[" 45","64","387","23"],["  6","98","215","314"]]
parseMatrix :: String -> [[String]]
parseMatrix = map parseLine . lines

-- >>> chunksOf 3 [1..25]
-- [[1,2,3],[4,5,6],[7,8,9],[10,11,12],[13,14,15],[16,17,18],[19,20,21],[22,23,24],[25]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

foo =
  "123 328  51 64 \n\
  \ 45 64  387 23 \n\
  \  6 98  215 314\n\
  \ *   +   *   + "

-- splitWithCorrectSpaces :: String -> [String]
-- splitWithCorrectSpaces st | words !! n ==' ' && words !! n+1 == ' ' = stringLength = length words !! i + 2
--

-- >>> f "+" 5 10
applyOp :: [Int] -> String -> Int
applyOp xs "*" = product xs
applyOp xs "+" = sum xs
applyOp _ op = error ("Unknown operator: " ++ op)

-- 123 45 6

-- >>> solution [[123, 328, 51, 64], [45, 64, 387, 23], [6, 98, 215, 314]] ["*", "+", "*", "+"]
-- >>> sum [33210,490,4243455,401]
solution :: [[Int]] -> [String] -> [Int]
solution rows = zipWith applyOp (transpose rows)

stringsToInts :: [[String]] -> [[Int]]
stringsToInts = map (map read)

main = do
  contents <- readFile "Day_6/input.txt"
  -- let contents = foo
  let ls = lines contents
  let parsed = map words ls
  let ints = take (length parsed - 1) parsed
  let parsedInts = stringsToInts ints
  let operators = parsed !! (length parsed - 1)
  print $ sum $ solution parsedInts operators

-- print $ length ints
-- print ints
-- print operators
