module Laboratories where

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

-- 1 = beam, 2 = splitter, 0 = empty
startBeams :: Int -> Int -> Int -> Int
startBeams self before next
  | before == 2 || next == 2 = 1
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

-- >>> zipWith extendBeams [0,0,2,0,0,0,0] [0,0,1,2,1,0,0]
-- [0,0,1,0,1,0,0]
extendBeams :: Int -> Int -> Int
extendBeams self up
  | up == 1 = max self 1
  | up == 2 = self
  | otherwise = self

-- >>> extendBeams 1 ([head bareTree]) bareTree
extendTreeBeams :: [[Int]] -> [[Int]]
extendTreeBeams m = go 1 [head m]
 where
  go row acc
    | length acc == length m = acc
    | otherwise =
        let newRow = zipWith extendBeams (m !! row) (acc !! (row - 1))
         in go (row + 1) (acc ++ [newRow])

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
  let fullTree = extendTreeBeams $ buildTree input
  putStrLn $ render fullTree
  print $ countSplits fullTree
