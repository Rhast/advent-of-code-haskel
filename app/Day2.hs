module Day2 (main) where

data Shape = Rock | Paper | Scissors
  deriving (Show, Enum, Bounded)

shapeScore :: Shape -> Int
shapeScore shape = case shape of
  Rock -> 1
  Paper -> 2
  Scissors -> 3

parseShape :: String -> Shape
parseShape str = case str of
  "A" -> Rock
  "B" -> Paper
  "C" -> Scissors
  _ -> error "Invalid shape"

data TurnResult = Win | Lose | Draw
  deriving (Show, Enum, Bounded)

parseResult :: String -> TurnResult
parseResult str = case str of
  "X" -> Lose
  "Y" -> Draw
  "Z" -> Win
  _ -> error "Invalid result"

shapeToGetResult :: TurnResult -> Shape -> Shape
shapeToGetResult result givenShape = case result of
  Win -> case givenShape of
    Rock -> Paper
    Paper -> Scissors
    Scissors -> Rock
  Lose -> case givenShape of
    Rock -> Scissors
    Paper -> Rock
    Scissors -> Paper
  Draw -> givenShape

turnResultScore :: TurnResult -> Int
turnResultScore result = case result of
  Win -> 6
  Lose -> 0
  Draw -> 3

data Turn = Turn {
  opponentShape :: Shape,
  myShape :: Shape
} deriving (Show)

parseTurn :: [String] -> Turn
parseTurn [opponentShapeStr, turnResultStr] = 
  let opponentShape = parseShape opponentShapeStr
      turnResult = parseResult turnResultStr
      myShape = shapeToGetResult turnResult opponentShape
  in Turn {opponentShape = opponentShape, myShape = myShape}

turnResult :: Turn -> TurnResult
turnResult turn =
  case (opponentShape turn, myShape turn) of
    (Rock, Rock) -> Draw
    (Rock, Paper) -> Win
    (Rock, Scissors) -> Lose
    (Paper, Rock) -> Lose
    (Paper, Paper) -> Draw
    (Paper, Scissors) -> Win
    (Scissors, Rock) -> Win
    (Scissors, Paper) -> Lose
    (Scissors, Scissors) -> Draw

score :: Turn -> Int
score turn = (shapeScore $ myShape turn) + (turnResultScore $ turnResult turn)

main :: IO ()
main = do
  content <- readFile "app/input.txt"
  let lineScore = score . parseTurn . words
  print $ sum $ map lineScore $ lines content
