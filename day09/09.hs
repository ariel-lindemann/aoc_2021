import Data.List (intersect)
import Data.Char ( digitToInt ) 
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)

data Point = Point {value :: Int, coordinates :: (Int, Int)} deriving (Eq, Show)
type Basin = Set Point

isMinimum :: Point -> [[Int]] -> Bool
isMinimum p grid = value p <= minimum (map value (neighbors p grid))

toPoints :: [[Int]] -> [Point]
toPoints grid = [Point{value = grid !! x !! y, coordinates = (x, y)} | x <- [0..(length grid - 1)], y <- [0..(length (head grid) - 1)]]

neighbors :: Point -> [[Int]] -> [Point]
neighbors p grid = [Point {value = grid !! x' !! y', coordinates = (x', y')} | x' <- [x -1 .. x + 1] `intersect` [0 .. x_max], y' <- [y -1 .. y + 1] `intersect` [0 .. y_max]] 
  where
    x = fst $ coordinates p
    y = snd $ coordinates p
    x_max = length grid - 1
    y_max = length (head grid) - 1

sumRiskLevels :: [[Int]] -> Int
sumRiskLevels grid = sum $ map (\p -> value p + 1) minima
  where minima = filter (`isMinimum` grid) $ toPoints grid

main :: IO ()
main = do
  args <- getArgs 
  contents <- readFile $ head args

  let grid = map (map digitToInt) $ lines contents
  print $ sumRiskLevels grid
