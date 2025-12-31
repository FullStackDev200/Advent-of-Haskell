module Movie_Theater where

foo =
  [ "7,1"
  , "11,1"
  , "11,7"
  , "9,7"
  , "9,5"
  , "2,5"
  , "2,3"
  , "7,3"
  ]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
   where
    (w, s'') = break p s'

-- >>> parseInput "3,5"
-- (3,5)
parseInput :: String -> (Integer, Integer)
parseInput s =
  let parts = wordsWhen (== ',') s
   in case parts of
        [start, end] -> (read start :: Integer, read end :: Integer)
        _ -> error ("Invalid range format: " ++ s)

-- >>> findS (2,5) (11,1)
-- 50
findS :: (Integer, Integer) -> (Integer, Integer) -> Integer
findS (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

-- >>> findMaxS $ result $ map parseInput foo
-- 50
result :: [(Integer, Integer)] -> [[Integer]]
result input = map (\x -> map (findS x) input) input

findMaxS :: [[Integer]] -> Integer
findMaxS = maximum . concat

-- >>> map (findS (7,1)) $ map parseInput foo
-- [1,4,24,12,8,20,10,2]
--

main = do
  contents <- readFile "Day_8/input.txt"
  let ls = lines contents
  -- let ls = foo
  let bar = findMaxS $ result $ map parseInput ls
  print bar
