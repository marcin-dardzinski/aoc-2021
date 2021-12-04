import System.Environment (getArgs)

data Command = Up Int | Down Int | Forward Int

data Position = Position
  { depth :: Int,
    horizontal :: Int,
    aim :: Int
  }

parseCommand :: String -> Command
parseCommand = parseInner . parseRaw . words
  where
    parseRaw :: [String] -> (String, Int)
    parseRaw [cmd, val] = (cmd, read val :: Int)
    parseRaw _ = error "Invalid command"

    parseInner :: (String, Int) -> Command
    parseInner ("up", x) = Up x
    parseInner ("down", x) = Down x
    parseInner ("forward", x) = Forward x
    parseInner _ = error "Invalid command"

applyCommand :: Position -> Command -> Position
applyCommand pos@Position {depth = d} (Up val) = pos {depth = d - val}
applyCommand pos@Position {depth = d} (Down val) = pos {depth = d + val}
applyCommand pos@Position {horizontal = h} (Forward val) = pos {horizontal = h + val}

applyCommand' :: Position -> Command -> Position
applyCommand' pos@(Position d h a) (Up val) = pos {aim = a - val}
applyCommand' pos@(Position d h a) (Down val) = pos {aim = a + val}
applyCommand' pos@(Position d h a) (Forward val) = pos {horizontal = h + val, depth = d + a * val}

applyCommands :: (Position -> Command -> Position) -> [Command] -> Position
applyCommands f = foldl f Position {depth = 0, horizontal = 0, aim = 0}

positionVal Position {depth = d, horizontal = h} = d * h

partOne = positionVal . applyCommands applyCommand

partTwo = positionVal . applyCommands applyCommand'

main = do
  args <- getArgs
  let file = if null args then "input.txt" else head args
  text <- readFile file
  let commands = map parseCommand $ lines text
  let p1 = partOne commands
  let p2 = partTwo commands
  print p1
  print p2