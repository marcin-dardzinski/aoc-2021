import Data.List
import System.Environment (getArgs)

parseDigits :: String -> [[Bool]]
parseDigits = map (map mapC) . lines
  where
    mapC '0' = False
    mapC '1' = True
    mapC _ = error "Unexpected character"

parseBinary :: [Bool] -> Int
parseBinary digits = sum $ zipWith (\b v -> if b then v else 0) (reverse digits) powers
  where
    powers = map (2 ^) $ iterate (+ 1) 0

mostCommonBit :: [Bool] -> Bool
mostCommonBit = uncurry toBit . foldl increase (0, 0)
  where
    increase (f, t) False = (f + 1, t)
    increase (f, t) True = (f, t + 1)
    toBit f t = t >= f

mostCommonBits :: [[Bool]] -> [Bool]
mostCommonBits = map mostCommonBit . transpose

partOne :: [[Bool]] -> Int
partOne x = gamma * eps
  where
    mcb = mostCommonBits x
    gamma = parseBinary mcb
    eps = parseBinary $ map not mcb

data Digit = Digit
  { all :: [Bool],
    remaining :: [Bool]
  }

filterByBits :: [[Bool]] -> Bool -> [Bool]
filterByBits digits high = reduce (map (\d -> Digit d d) digits) (transpose digits)
  where
    reduce :: [Digit] -> [[Bool]] -> [Bool]
    reduce [Digit all _] _ = all
    reduce ds (b : bs) = reduce filtered (transpose $ map remaining filtered)
      where
        mcb = mostCommonBit b
        bitMatch (Digit all (x : xs)) = if high then x == mcb else x /= mcb
        bitMatch _ = False
        filtered = map skipDigit $ filter bitMatch ds
    reduce [] _ = error "Empty digits list"
    reduce _ [] = error "Empty bits list"

skipDigit :: Digit -> Digit
skipDigit (Digit all (x : xs)) = Digit all xs
skipDigit _ = error "Unexpected end of remaining digits"

partTwo :: [[Bool]] -> Int
partTwo digits =
  let oxygen = parseBinary $ filterByBits digits True
      co2 = parseBinary $ filterByBits digits False
   in oxygen * co2

main = do
  args <- getArgs
  let file = if null args then "sample.txt" else head args
  text <- readFile file
  let commands = parseDigits text
  let p1 = partOne commands
  let p2 = partTwo commands
  print p1
  print p2