import Data.List (transpose)
import Data.Set (Set, empty, insert, member, notMember, singleton)
import Data.Text (Text, empty, pack, split, splitOn, unpack)
import System.Environment

data Board = Board
  { rows :: [[Int]],
    columns :: [[Int]]
  }
  deriving (Show)

data BingoResult = BingoResult
  { winner :: Board,
    lastDrawn :: Int,
    drawn :: Set Int
  }
  deriving (Show)

parseBoardsAndDraws :: String -> ([Board], [Int])
parseBoardsAndDraws text = (boards, draws)
  where
    t = splitOn (pack "\n\n") $ pack text
    rawDraws = head t
    rawBoards = tail t
    draws = parseDraws rawDraws
    boards = map parseBoard rawBoards
    parseInt :: Text -> Int
    parseInt = read . unpack
    parseDraws = map parseInt . split (== ',')
    parseBoard t = Board {rows = rows, columns = transpose rows}
      where
        rows = filter (not . null) $ map parseRow $ split (== '\n') t
        parseRow :: Text -> [Int]
        parseRow = map parseInt . filter (/= Data.Text.empty) . split (== ' ')

headMaybe (x : xs) = Just x
headMaybe _ = Nothing

tryGetWinner :: Set Int -> [Board] -> Maybe Board
tryGetWinner draws = headMaybe . filter (isWinning draws)

isWinning :: Set Int -> Board -> Bool
isWinning draws (Board row column) = any (all (`member` draws)) (row ++ column)

bingoFirst :: [Board] -> [Int] -> BingoResult
bingoFirst boards = reduce Data.Set.empty
  where
    reduce :: Set Int -> [Int] -> BingoResult
    reduce set (d : ds) =
      let newSet = insert d set
       in case tryGetWinner newSet boards of
            Just b -> BingoResult {winner = b, lastDrawn = d, drawn = newSet}
            Nothing -> reduce newSet ds
    reduce _ [] = error "Unexpected end of draws"

bingoFirst' :: [Board] -> [Int] -> BingoResult
bingoFirst' b d = head $ bingoAll b d

bingoAll :: [Board] -> [Int] -> [BingoResult]
bingoAll = reduce Data.Set.empty
  where
    reduce :: Set Int -> [Board] -> [Int] -> [BingoResult]
    reduce _ [] _ = []
    reduce _ _ [] = []
    reduce set boards (d : ds) = winning ++ reduce newSet losing ds
      where
        newSet = insert d set
        winning = map mkResult $ filter (isWinning newSet) boards
        losing = filter (not . isWinning newSet) boards
        mkResult b = BingoResult {winner = b, lastDrawn = d, drawn = newSet}

        split = span (isWinning newSet) boards

bingoLast :: [Board] -> [Int] -> BingoResult
bingoLast b d = last $ bingoAll b d

score :: BingoResult -> Int
score (BingoResult (Board rows _) lastDrawn drawn) = lastDrawn * (sum . filter (`notMember` drawn) $ concat rows)

partOne = score . uncurry bingoFirst

partOne' = score . uncurry bingoFirst'

partTwo = score . uncurry bingoLast

main = do
  args <- getArgs
  let file = if null args then "input.txt" else head args
  text <- readFile file
  let board = parseBoardsAndDraws text

  print $ partOne board
  print $ partOne' board
  print $ partTwo board