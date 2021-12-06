import System.Environment (getArgs)
import Data.Set (fromList)
import Data.List (group, sort)
type Point = (Int, Int)
type Line = (Point, Point)
type Grid = [[Int]]

pointsInLine :: Line -> [Point]
pointsInLine ((x1, y1), (x2, y2))
  | x1 == x2 = [(x1, y') | y' <- inters y1 y2]
  | y1 == y2 = [(x', y1) | x' <- inters x1 x2]
  | otherwise = []
  where inters a b
          | a > b = [b..a]
          | otherwise = [a..b]

pointsOccurence :: Int -> [Point] -> Int
pointsOccurence n ps = length $ (map head . filter (\x -> length x >= n) . group . sort) ps

intersectionGrid :: [Point] -> Grid -> Grid
intersectionGrid ((x,y):ps) g = intersectionGrid ps g'
  where g' = undefined
intersectionGrid [] g = g

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

  let points = concatMap (pointsInLine . parseLine) (lines contents)
  print 2
  print $ pointsOccurence 2 points