{-# LANGUAGE FlexibleContexts #-}

module TicTacToe where

import Data.List (elemIndices)
import Data.Maybe (isNothing)

-- import System.Random (randomRIO)

newtype Player = Player Char deriving (Show)

instance Eq Player where
  (==) (Player x) (Player y) = x == y

data Move = Move Int Int deriving (Show)

type Board = [Maybe Player]

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
-- >>> createPlayer 'X'
-- Player 'X'
--
-- >>> createPlayer 'O'
-- Player 'O'
createPlayer :: Char -> Player
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
translateCoords (Move row column)
  | row > 3 || column > 3 = Nothing
  | otherwise = Just ((row - 1) * 3 + (column - 1))

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
translateIndex index
  | index > 8 = Nothing
  | otherwise = do
      let (row, col) = divMod index 3
      Just (Move (row + 1) (col + 1))

-- | Replace an empty square with Player
-- Examples:
-- >>> placePlayersMark [Nothing, Just (Player 'X')] 0 (Just (Player 'O'))
-- Just [Just (Player 'O'),Just (Player 'X')]
--
-- >>> placePlayersMark [Nothing, Just (Player 'X')] 1 (Just (Player 'O'))
-- Nothing
placePlayersMark :: Board -> Int -> Maybe Player -> Maybe Board
placePlayersMark board index player
  | isNothing (board !! index) = Just (take index board ++ [player] ++ drop (index + 1) board)
  | otherwise = Nothing

-- | Making a move on a board as a player
-- Examples:
-- >>> makeMove [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] (Move 1 2) (Player 'X')
-- Just [Nothing,Just (Player 'X'),Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
--
-- >>> makeMove [Nothing, Just (Player 'X'), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] (Move 1 2) (Player 'O')
-- Nothing
--
-- >>> makeMove [Nothing, Just (Player 'X'), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] (Move 4 3) (Player 'O')
-- Nothing
--
-- >>> makeMove [Nothing, Just (Player 'X'), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] (Move 3 4) (Player 'O')
-- Nothing
makeMove :: Board -> Move -> Player -> Maybe Board
makeMove board move player = do
  index <- translateCoords move
  placePlayersMark board index (Just player)

-- | Get a sequence of horizontal elements provided array and the number of steps to take
--
-- Examples:
-- >>> horizontalGenerator [0] 3
-- [0,1,2]
--
-- >>> horizontalGenerator [3] 3
-- [3,4,5]
horizontalGenerator :: [Int] -> Int -> [Int]
horizontalGenerator prevSteps size
  | length prevSteps == size = prevSteps
  | otherwise = horizontalGenerator (prevSteps ++ [last prevSteps + 1]) size

-- | Get a sequence of vertical elements provided array containing the first and the number of steps to take
--
-- Examples:
-- >>> verticalGenerator [0] 3
-- [0,3,6]
--
-- >>> verticalGenerator [3] 3
-- [3,6,9]
verticalGenerator :: [Int] -> Int -> [Int]
verticalGenerator prevSteps size
  | length prevSteps == size = prevSteps
  | otherwise = verticalGenerator (prevSteps ++ [last prevSteps + size]) size

-- | Get a sequence of diagonal elements provided array containing the first and the number of steps to take
--
-- Examples:
-- >>> diagonalGenerator [0] 3
-- [0,4,8]
diagonalGenerator :: [Int] -> Int -> [Int]
diagonalGenerator prevSteps size
  | length prevSteps == size = prevSteps
  | otherwise = diagonalGenerator (prevSteps ++ [last prevSteps + size + 1]) size

-- | Get a sequence of inverse diagonal elements provided array containing the first and the number of steps to take
--
-- Examples:
-- >>> inverseDiagonalGenerator [2] 3
-- [2,4,6]
inverseDiagonalGenerator :: [Int] -> Int -> [Int]
inverseDiagonalGenerator prevSteps size
  | length prevSteps == size = prevSteps
  | otherwise = inverseDiagonalGenerator (prevSteps ++ [last prevSteps + size - 1]) size

-- | Get a sequence of horizontal elements starting from some index
--
-- Examples:
-- >>> generateSteps horizontalGenerator 0 3
-- [0,1,2]
--
-- >>> generateSteps horizontalGenerator 3 3
-- [3,4,5]
generateSteps :: ([Int] -> Int -> [Int]) -> Int -> Int -> [Int]
generateSteps generator starting = generator [starting]

-- | Get positions of player given a board and a player that we are looking for
--
-- Examples:
-- >>> getPlayerPositions [Nothing,Just (Player 'X'),Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing] (Player 'X')
-- [1]
getPlayerPositions :: Board -> Player -> [Int]
getPlayerPositions board player = elemIndices (Just player) board

-- createRandomMove :: Float -> Board -> Move
-- createRandomMove seed board = do
--   let emptySquares = elemIndices Nothing board
--   emptySquares !! (seed - 1 / length emptySquares - 1)
