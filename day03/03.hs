import System.Environment (getArgs)
findBinary :: [[Int]] -> [Int]
findBinary l@(x:xs)
  | null x = []
  | otherwise = mostCommon (firsts l) : findBinary (map tail l)
findBinary _ = []

firsts :: [[a]] -> [a]
firsts ((x:xs):ys) = x:firsts ys
firsts _ = []

mostCommon :: [Int] -> Int
mostCommon l@(x:xs) = if length (filter (== x) l) > length l `div` 2 then x else ((x+1) `mod` 2)
mostCommon _ = 0

toDecimal :: [Int] -> Int
toDecimal [] = 0
toDecimal l = last l + 2 * toDecimal (init l)

parseBin :: String -> [Int]
parseBin (x:xs) = read [x] : parseBin xs
parseBin _ = []

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args

  let nums = map parseBin $ lines contents
  let gamma = findBinary nums
  let epsilon = map (\x -> (x + 1) `mod` 2) gamma
  print $ toDecimal gamma * toDecimal epsilon