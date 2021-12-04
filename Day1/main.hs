parseList :: String -> [Int]
parseList = map (\n -> read n :: Int) . lines

findIncreasing :: [Int] -> Int
findIncreasing all@(x : xs) = length $ filter (> 0) $ zipWith (-) xs all
findIncreasing _ = error "List have less then two elements"

partOne = findIncreasing

tupleSum a b c = a + b + c

partTwo :: [Int] -> Int
partTwo x = findIncreasing $ zipWith3 tupleSum (skip 2) (skip 1) x
  where
    skip n = drop n x

main = do
  text <- readFile "input.txt"
  let depths = parseList text
  let p1 = partOne depths
  let p2 = partTwo depths

  print p1
  print p2
