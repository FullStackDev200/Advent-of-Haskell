{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (split)
import Data.Char (digitToInt)
import Data.List (foldl, head)
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
-- 123
fromDigits :: [Integer] -> Integer
fromDigits = read . concatMap show

textToInteger :: T.Text -> Integer
textToInteger t = fromMaybe (error "Invalid number format") (readMaybe (T.unpack t))

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
   where
    (w, s'') = break p s'

-- >>> parseRange' "11-22"
-- (11,22)
parseRange :: String -> (Integer, Integer)
parseRange s =
  let parts = wordsWhen (== '-') s
   in case parts of
        [start, end] -> (read start :: Integer, read end :: Integer)
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

-- >>> hasRepeating [1,2,1,2]
-- 1212
hasRepeating :: [Integer] -> Integer
hasRepeating xs =
  if any isRepeat [1 .. n `div` 2]
    then fromDigits xs
    else 0
 where
  n = length xs

  isRepeat k =
    let a = take k xs
     in n `mod` k == 0
          && concat (replicate (n `div` k) a) == xs

-- >>> makeRange (11,33)
-- [11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33]
makeRange :: (Integer, Integer) -> [Integer]
makeRange (x, y) = [x .. y]

-- >>> solution "2121212118-2121212124"
-- 0
solution :: String -> Integer
solution = sum . map isHalfRepeating . makeRange . parseRange

-- >>> solution' "2121212118-2121212124"
-- 2121212121
solution' :: String -> Integer
solution' = sum . map (hasRepeating . digits) . makeRange . parseRange

main = do
  contents <- readFile "Day_2\\input.txt"
  let ls = lines contents
  print . sum $ map solution ls
