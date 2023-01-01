module TicTacToe where

import Data.List (elemIndices)
import Data.Maybe (isNothing)

-- import System.Random (randomRIO)
data Direction = Vertical | Horizontal | Diagonal | InverseDiagonal

newtype Player = Player Char deriving (Show)

instance Eq Player where
  (==) (Player x) (Player y) = (==) x y

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
-- >>> translateCoords (Move 1 1) 3
-- Just 0
--
-- >>> translateCoords (Move 2 2) 3
-- Just 4
--
-- >>> translateCoords (Move 3 1) 3
-- Just 6
translateCoords :: Move -> Int -> Maybe Int
translateCoords (Move row column) size
  | row > size || column > size = Nothing
  | otherwise = Just ((row - 1) * size + (column - 1))

-- | Translate index to coords
--
-- Examples:
-- >>> translateIndex 8 3
-- Just (Move 3 3)
--
-- >>> translateIndex 3 3
-- Just (Move 2 1)
--
-- >>> translateIndex 0 3
-- Just (Move 1 1)
translateIndex :: Int -> Int -> Maybe Move
translateIndex index size
  | index > 8 = Nothing
  | otherwise = do
      let (row, col) = divMod index size
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
  index <- translateCoords move (round (sqrt (fromIntegral (length board))))
  placePlayersMark board index (Just player)

-- | Get a sequence indices provided array containing the first, number of steps to take and direction
--
-- Examples:
-- >>> sequenceGenerator [0] 3 Horizontal
-- [0,1,2]
--
-- >>> sequenceGenerator [3] 3 Horizontal
-- [3,4,5]
--
-- >>> sequenceGenerator [0] 3 Vertical
-- [0,3,6]
--
-- >>> sequenceGenerator [3] 3 Vertical
-- [3,6,9]
--
-- >>> sequenceGenerator [0] 3 Diagonal
-- [0,4,8]
--
-- >>> sequenceGenerator [2] 3 InverseDiagonal
-- [2,4,6]
sequenceGenerator :: [Int] -> Int -> Direction -> [Int]
sequenceGenerator prevSteps size Horizontal
  | length prevSteps == size = prevSteps
  | otherwise = sequenceGenerator (prevSteps ++ [last prevSteps + 1]) size Horizontal
sequenceGenerator prevSteps size Vertical
  | length prevSteps == size = prevSteps
  | otherwise = sequenceGenerator (prevSteps ++ [last prevSteps + size]) size Vertical
sequenceGenerator prevSteps size Diagonal
  | length prevSteps == size = prevSteps
  | otherwise = sequenceGenerator (prevSteps ++ [last prevSteps + size + 1]) size Diagonal
sequenceGenerator prevSteps size InverseDiagonal
  | length prevSteps == size = prevSteps
  | otherwise = sequenceGenerator (prevSteps ++ [last prevSteps + size - 1]) size InverseDiagonal

generateSequence :: ([Int] -> Int -> [Int]) -> Int -> Int -> [Int]
generateSequence generator starting = generator [starting]

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
