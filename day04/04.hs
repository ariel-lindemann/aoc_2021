import Data.List (elemIndices, transpose)
import System.Environment (getArgs)

type Board = [[Int]]

isWinner :: Board -> Bool
isWinner b = findCompleteRows b || findCompleteCols b

findCompleteRows :: Board -> Bool
findCompleteRows (x:xs)
  | length (filter (==999) x) == length x = True
  | otherwise = findCompleteRows xs
findCompleteRows [] = False

findCompleteCols :: Board -> Bool
findCompleteCols b = findCompleteRows $ transpose b

markNumber :: Int -> Board -> Board
markNumber n = map (map f)
  where f x = if x==n then 999 else x

calculateScore :: Board -> Int
calculateScore b = sum $ map sum $ filtered b
  where filtered = map $ filter (/= 999)

scoreOfFirst :: [Int] -> [Board] -> Int
scoreOfFirst ns bs = fst winingConfig * calculateScore (snd winingConfig)
  where winingConfig = findWinner bs ns

scoreOfLast :: [Int] -> [Board] -> Int
scoreOfLast ns bs = fst losingConfig * calculateScore (snd losingConfig)
  where losingConfig = findLoser bs ns

findWinner :: [Board] -> [Int] -> (Int, Board)
findWinner bs (n:ns)
  | or $ fst winners = (n, snd winners)
  | otherwise = findWinner nextBoards ns
  where winners = (map isWinner nextBoards, head $ filter isWinner nextBoards)
        nextBoards = map (markNumber n) bs
findWinner _ [] = error "no winners"

findLoser :: [Board] -> [Int] -> (Int, Board)
findLoser bs (n:ns)
  | and $ fst losers = (n, snd losers)
  | otherwise = findLoser nextBoards ns
  where losers = (map isWinner nextBoards, nextBoards !! head loserIndex)
        nextBoards = map (markNumber n) bs
        loserIndex = elemIndices False (map isWinner bs)
findLoser _ [] = error "no losers"

parseBoard :: [String] -> Board
parseBoard = map nums
  where nums l = map read $ words l

paragraphs :: String -> [String]
paragraphs "" = []
paragraphs "\n" = []
paragraphs ('\n':'\n':cs) = "\n":paragraphs cs
paragraphs (c:cs) = case paragraphs cs of
    [] -> [[c, '\n']]
    p:ps -> (c:p):ps

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'


main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args

  let inputLines = lines contents
  let nums = map read $ wordsWhen (==',') $ head inputLines
  let boards = map (parseBoard . lines) (tail $ paragraphs contents)

  --part 1
  print $ scoreOfFirst nums boards

  --part 2
  print $ scoreOfLast nums boards
