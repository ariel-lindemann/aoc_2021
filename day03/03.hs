import System.Environment (getArgs)
findBinary :: [[Int]] -> [Int]
findBinary l@(x:xs)
  | null x = []
  | otherwise = mostCommon (nth l 0) : findBinary (map tail l)
findBinary _ = []

nth :: [[a]] -> Int -> [a]
nth l i= [k!!i | k <- l]

flipBit :: Int -> Int
flipBit x = (x + 1) `mod` 2

mostCommon :: [Int] -> Int
mostCommon l@(x:_)
  | r == 0 = 1
  | r < 0 = x
  | otherwise = flipBit x
  where r = length (filter (/=x) l) - length (filter (==x) l)
mostCommon _  = 0

toDecimal :: [Int] -> Int
toDecimal [] = 0
toDecimal l = last l + 2 * toDecimal (init l)

parseBin :: String -> [Int]
parseBin (x:xs) = read [x] : parseBin xs
parseBin _ = []

filterCriteria :: [[Int]] -> Int -> Bool -> [Int]
filterCriteria l i com
  | length l <= 1 = head l
  | otherwise = filterCriteria (filter (f i com) l) (i + 1) com
  where f i com l'
          | com = l'!!i == mostCommon (nth l i)
          | otherwise = l'!!i /= mostCommon (nth l i)

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args

  let nums = map parseBin $ lines contents

  --part 1
  let gamma = findBinary nums
  let epsilon = map flipBit gamma
  print $ toDecimal gamma * toDecimal epsilon

  --part 2
  let oxygen = toDecimal $ filterCriteria nums 0 True
  let co2 = toDecimal $ filterCriteria nums 0 False

  print $ oxygen * co2