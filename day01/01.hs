import System.Environment (getArgs)

increased :: (Num a, Ord a) => a -> a -> Bool
increased x y = x < y

countIncrease :: (Num a, Ord a) => a -> a -> [a] -> a
countIncrease n _ [] = n
countIncrease n prev (x:xs)
  | increased prev x = countIncrease (n+1) x xs
  | otherwise = countIncrease n x xs

nSums :: (Num a) => Int -> [a] -> [a] -> [a]
nSums n ss [] = ss
nSums n ss (x:xs)
  | length (x:xs) < n = ss
  | otherwise = nSums n (ss++[sum (take n (x:xs))]) xs

main :: IO ()
main = do
  args <- getArgs 
  content <- readFile $ head args

  --part 1
  let increases = countIncrease 0 10000 $ map read $ lines content
  print increases

  --part 2
  let threeIncreases = countIncrease 0 10000 $ nSums 3 [] $ map read $ lines content
  print threeIncreases