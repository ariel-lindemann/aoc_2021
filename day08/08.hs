import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)

data Decode = Decode {right :: Set Char, topL :: Set Char, bottomL :: Set Char} deriving (Show)

switchKeyPair :: Ord b => Map a b -> Map b a
switchKeyPair m = Map.fromList [(y, x) | (x, y) <- Map.toList m]

listToInt :: [Int] -> Int
listToInt = foldr (\x acc -> 10 * acc + x) 0

decodeDigits :: Map (Set Char) Int -> [String] -> [Int]
decodeDigits digitMap = map (\s -> Map.findWithDefault 0 (Set.fromList s) digitMap)

createDigitMap :: [String] -> Map (Set Char) Int
createDigitMap ss =
  let es = Set.fromList $ filter (\x -> length x `elem` [2, 4, 3, 7]) ss
      esMap = switchKeyPair $ Map.unions $ Set.map easyDigitMap es
      basicDecoder = createEasyDecoder $ Map.unions $ Set.map easyDigitMap es
      ds = map Set.fromList $ filter (`notElem` es) ss
      dsMap = Map.fromList [(d, decodeNonEasylDig basicDecoder d) | d <- ds]
   in Map.union esMap dsMap

easyDigitMap :: String -> Map Int (Set Char)
easyDigitMap d = case length d of
  2 -> Map.fromList [(1, Set.fromList d)]
  4 -> Map.fromList [(4, Set.fromList d)]
  3 -> Map.fromList [(7, Set.fromList d)]
  7 -> Map.fromList [(8, Set.fromList d)]
  _ -> error "not a trivial digit"

-- we can deduce every number if we know the right two bars or one of the pairs of
-- bars which make up the two L shapes
createEasyDecoder :: Map Int (Set Char) -> Decode
createEasyDecoder easyDigsMap =
  Decode
    { right = f 1,
      bottomL = deduceBottomL (f 4) (f 7) (f 8),
      topL = deduceTopL (f 1) (f 4)
    }
  where
    f x = Map.findWithDefault Set.empty x easyDigsMap

-- needs 4, 7 and 8 as input
deduceBottomL :: Set Char -> Set Char -> Set Char -> Set Char
deduceBottomL four seven eight = Set.difference eight (Set.union four seven)
  where
    s x = Set.fromList x

-- needs 1 and 4 as input
deduceTopL :: Set Char -> Set Char -> Set Char
deduceTopL one four = Set.difference four one

decodeNonEasylDig :: Decode -> Set Char -> Int
decodeNonEasylDig d s = case length s of
  5 -> identifyLengh5 d s
  6 -> identifyLengh6 d s
  _ -> error "wrong length"

identifyLengh5 :: Decode -> Set Char -> Int
identifyLengh5 d s
  | Set.size (bottomL d `Set.intersection` s) == 2 = 2
  | Set.size (right d `Set.intersection` s) == 2 = 3
  | otherwise = 5

identifyLengh6 :: Decode -> Set Char -> Int
identifyLengh6 d s
  | Set.size (bottomL d `Set.intersection` s) == 1 = 9
  | Set.size (right d `Set.intersection` s) == 2 = 0
  | otherwise = 6

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args

  let contentsLines = lines contents
  let inputValuesByLines = concatMap (map words . init . splitOn "|") contentsLines
  let outputValuesByLines = concatMap (map words . tail . splitOn "|") contentsLines
  let allValues = inputValuesByLines ++ outputValuesByLines
  let trivialsOnly l = map (filter (\x -> length x `elem` [2, 4, 3, 7])) outputValuesByLines

  --part 1
  print $ length $ concat $ trivialsOnly outputValuesByLines

  -- part 2
  let digitMaps = map createDigitMap allValues
  let decodeOutputs i = (decodeDigits $ digitMaps !! i) (outputValuesByLines !! i)
  let decodedOutputs = map (listToInt . reverse . decodeOutputs) [0 .. (length inputValuesByLines - 1)]
  print $ sum decodedOutputs
  