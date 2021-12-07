import System.Environment (getArgs)
import qualified Data.Map
import Data.Map (Map)
import Data.List (map, group, sort)
import Data.List.Split (splitOn)

type Fish = Int
type Cohort = Map Fish Int

nextDay :: Cohort -> Cohort
nextDay fs = Data.Map.mapWithKey (\key n -> updateAmount key fs) fs
  where updateAmount 8 fs = Data.Map.findWithDefault 0 0 fs
        updateAmount 6 fs = Data.Map.findWithDefault 0 7 fs + Data.Map.findWithDefault 0 0 fs
        updateAmount n fs = Data.Map.findWithDefault 0 (n+1) fs

nDays :: Cohort -> Int -> Cohort
nDays c n = iterate nextDay c !! n

totalFishes :: Cohort -> Int
totalFishes = Data.Map.foldr (+) 0

toCohort :: [Fish] -> Cohort
toCohort fishes = initializeCohort $ Data.Map.fromList $ Data.List.map (\n -> (head n, length n)) ((group . sort) fishes)
  where initializeCohort c = Data.Map.unionWith (+) c (Data.Map.fromList (zip [0..] (replicate 9 0)))

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args

  let fishes = map read $ splitOn "," contents
  let cohort = toCohort fishes

  --part1
  print $ totalFishes $ nDays cohort 80
  --part2
  print $ totalFishes $ nDays cohort 256