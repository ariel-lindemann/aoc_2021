import System.Environment (getArgs)
import Data.List (group, sort)

type Point = (Int, Int)
type Line = (Point, Point)
type Grid = [[Int]]

pointsInLine :: Bool -> Line -> [Point]
pointsInLine diag ((x1, y1), (x2, y2))
  | x1 == x2 = [(x1, y') | y' <- inters y1 y2]
  | y1 == y2 = [(x', y1) | x' <- inters x1 x2]
  | otherwise = if diag then dPoints else []
  where inters a b
          | a > b = [b..a]
          | otherwise = [a..b]
        dPoints = [(x1 + sX * d, y1 + sY * d) | d <- [0..(abs (x1-x2))]]
        sX = signum (x2-x1)
        sY = signum (y2-y1)

pointsOccurence :: Int -> [Point] -> Int
pointsOccurence n ps = length $ (map head . filter (\x -> length x >= n) . group . sort) ps

parseLine :: String -> Line
parseLine s = (parsePoint $ head p, parsePoint $ last p)
  where p = words s

parsePoint :: String -> Point
parsePoint x = (\[a,b] -> (a,b)) $ map read (wordsWhen (==',') x)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'


main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args

  --part1
  let points = concatMap (pointsInLine False . parseLine) (lines contents)
  print $ pointsOccurence 2 points

  --part2
  let diagPoints = concatMap (pointsInLine True . parseLine) (lines contents)
  print $ pointsOccurence 2 diagPoints