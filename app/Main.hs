module Main where

import Data.List (elemIndices)

-- import System.Random (randomRIO)

newtype Player = Player Int deriving (Show)

data Move = Move Int Int deriving (Show)

type Board = [Maybe Int]

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
createPlayer :: Int -> Player
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
translateCoords :: Move -> Maybe Int
translateCoords (Move row column) =
  if row > 3 || column > 3
    then Nothing
    else Just ((row - 1) * 3 + (column - 1))

-- | Translate index to coords
--
-- Examples:
-- >>> translateIndex 8
-- Just (Move 3 3)
--
-- >>> translateIndex 3
-- Just (Move 2 1)
--
-- >>> translateIndex 0
-- Just (Move 1 1)
translateIndex :: Int -> Maybe Move
translateIndex index =
  if index > 8
    then Nothing
    else do
      let (row, col) = divMod index 3
      Just (Move (row + 1) (col + 1))

-- | Replace an element at index in list
-- Examples:
-- >>> replaceAtIndex [1,2,3,4] 2 9
-- [1,2,9,4]
replaceAtIndex :: [a] -> Int -> a -> [a]
replaceAtIndex xs i x = take i xs ++ [x] ++ drop (i + 1) xs

-- | Making a move on a board as a player
-- Examples:
-- >>> makeMove [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] (Move 1 2) (Player 1)
-- [Nothing,Just 1,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
--
-- >>> makeMove [Nothing, Just 1, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] (Move 1 2) (Player 2)
-- [Nothing,Just 1,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
--
-- >>> makeMove [Nothing, Just 1, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] (Move 4 3) (Player 2)
-- [Nothing,Just 1,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
--
-- >>> makeMove [Nothing, Just 1, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] (Move 3 4) (Player 2)
-- [Nothing,Just 1,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
makeMove :: Board -> Move -> Player -> Board
makeMove board move (Player player_num) =
  case translateCoords move of
    Nothing -> board
    Just index ->
      case board !! index of
        Just _ -> board
        Nothing -> replaceAtIndex board index (Just player_num)

-- createRandomMove :: Float -> Board -> Move
-- createRandomMove seed board = do
--   let emptySquares = elemIndices Nothing board
--   emptySquares !! (seed - 1 / length emptySquares - 1)

main :: IO ()
main = do
  let emptyBoard = createEmptyBoard
  print emptyBoard
