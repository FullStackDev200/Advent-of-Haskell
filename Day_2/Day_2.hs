{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (split)
import Data.Char (digitToInt)
import Data.List (foldl)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Text.Read (Lexeme (String), readMaybe)

foo :: [String] =
  [ "11-22"
  , "95-115"
  , "998-1012"
  , "1188511880-1188511890"
  , "222220-222224"
  , "1698522-1698528"
  , "446443-446449"
  , "38593856-38593862"
  , "565653-565659"
  , "824824821-824824827"
  , "2121212118-2121212124"
  ]

-- >>> digits 11
-- [1,1]
digits :: Integer -> [Integer]
digits = map (read . return) . show

-- >>> both (+1) (1,2)
-- (2,3)
both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

-- >>> fromDigits [1,2,3]
fromDigits :: [Integer] -> Integer
fromDigits = foldl (\acc d -> acc * 10 + d) 0

-- >>> flattenTuple ([1], [1])
-- (1,1)
flattenTuple :: ([a], [b]) -> (a, b)
flattenTuple (x : _, y : _) = (x, y)
flattenTuple _ = error "Empty list"

textToInteger :: T.Text -> Integer
textToInteger t = fromMaybe (error "Invalid number format") (readMaybe (T.unpack t))

-- >>> parseInt "11-22"
-- (11,22)
parseRange :: String -> (Integer, Integer)
parseRange s =
  let parts = T.splitOn "-" (T.pack s)
   in case parts of
        [start, end] -> (textToInteger start, textToInteger end)
        _ -> error ("Invalid range format: " ++ s)

-- >>> isHalfRepeating 1212
-- 1212
isHalfRepeating :: Integer -> Integer
isHalfRepeating x
  | odd (length (digits x)) = 0
  | otherwise =
      let middle = length (digits x) `div` 2
          (a, b) = splitAt middle (digits x)
       in if fromDigits a == fromDigits b
            then x
            else 0
 where
  ds = digits x

-- >>> makeRange (11,33)
-- [11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33]
makeRange :: (Integer, Integer) -> [Integer]
makeRange (x, y) = [x .. y]

-- >>> solution "1111-1130"
-- 1111
solution :: String -> Integer
solution a = sum $ map isHalfRepeating $ makeRange (parseRange a)

main = do
  contents <- readFile "Day_2\\input.txt"
  let ls = lines contents
  print . sum $ map solution ls
