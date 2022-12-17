module Main where

newtype Player = Player Integer deriving (Show)

data Move = Move Integer Integer deriving (Show)

type Board = [Maybe Integer]

-- | Create an empty board
--
-- Examples:
-- >>> createEmptyBoard
-- [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
createEmptyBoard :: Board
createEmptyBoard = replicate 9 Nothing

-- | Create a Player
--
-- Examples:
-- >>> createPlayer 1
-- Player 1
--
-- >>> createPlayer 2
-- Player 2
createPlayer :: Integer -> Player
createPlayer = Player

-- | Translate coords from points to index
--
-- Examples:
-- >>> translateCoords (Move 1 1)
-- Just 0
--
-- >>> translateCoords (Move 2 2)
-- Just 4
--
-- >>> translateCoords (Move 3 1)
-- Just 6
translateCoords :: Move -> Maybe Integer
translateCoords (Move row column) =
  if row > 3 || column > 3
    then Nothing
    else Just ((row - 1) * 3 + (column - 1))

makeMove :: Move -> Board -> Player -> Board
makeMove _ board _ = board

-- createRandomMove :: Board -> Move

main :: IO ()
main = do
  let emptyBoard = createEmptyBoard
  print emptyBoard
