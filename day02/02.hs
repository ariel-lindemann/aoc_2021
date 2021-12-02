import System.Environment (getArgs)

type Position = (Int, Int)
type Configuration = (Int, Int, Int)

data Direction = Forward | Up | Down deriving Show
data Command = Command {direction :: Direction, value :: Int} deriving Show


updatePosition :: Position -> [Command] -> Position
updatePosition p [] = p
updatePosition p@(x,y) (c:cs) = updatePosition p' cs
  where p' = case direction c of
          Forward -> (x + value c, y)
          Up -> (x, y - value c)
          Down -> (x, y + value c)

updateConfiguration :: Configuration -> [Command] -> Configuration
updateConfiguration p [] = p
updateConfiguration p@(x,y,a) (c:cs) = updateConfiguration p' cs
  where p' = case direction c of
          Forward -> (x + value c, y + a * value c, a)
          Up -> (x, y, a - value c)
          Down -> (x, y, a + value c)

parseCommand :: String -> Command
parseCommand s = Command{direction = d, value=  read $ last $ words s}
  where d = case head $ words s of
          "forward" -> Forward
          "up" -> Up
          _ -> Down

multiplyTwo :: (Num a) => (a, a, a) -> a
multiplyTwo (x, y, _) = x * y

main :: IO ()
main = do
  args <- getArgs
  content <- readFile $ head args

  --part 1
  let commands = map parseCommand $ lines content
  let finalPos = updatePosition (0,0) commands
  print $ uncurry (*) finalPos

  --part 2
  let finalPos' = updateConfiguration (0,0,0) commands
  print $ multiplyTwo finalPos'