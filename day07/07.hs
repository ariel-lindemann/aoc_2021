import Data.List.Split (splitOn)
import System.Environment (getArgs)

fuelToPos :: Int -> [Int] -> Int
fuelToPos n = foldr (\x acc-> acc + abs (x-n)) 0

fuelToPos' :: Int -> [Int] -> Int
fuelToPos' n = foldr f 0
  where f x acc = acc + ((distance x + 1) * distance x) `div` 2 
        distance x = abs (x-n)

fuelToEach :: Bool -> [Int] -> [Int]
fuelToEach linearIncrease p = map (`fuelFunction` p) [0..(length p)]
  where fuelFunction = if linearIncrease then fuelToPos else fuelToPos'

mostEfficientPos :: Bool -> [Int] -> Int 
mostEfficientPos linearIncrease ps = minimum $ fuelToEach linearIncrease ps

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args

  let positions = map read $ splitOn "," contents
  print $ mostEfficientPos True positions
  print $ mostEfficientPos False positions