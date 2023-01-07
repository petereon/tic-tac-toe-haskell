module TicTacToe where

import Data.List (elemIndex, elemIndices)
import Data.Maybe (isNothing)

-- import System.Random (randomRIO)
data Direction = Vertical | Horizontal | Diagonal | InverseDiagonal

newtype Player = Player Char deriving (Show)

instance Eq Player where
  (==) (Player x) (Player y) = (==) x y

data Move = Move Int Int deriving (Show)

getX :: Maybe Move -> Maybe Int
getX (Just (Move x _)) = Just x
getX Nothing = Nothing

getY :: Maybe Move -> Maybe Int
getY (Just (Move _ y)) = Just y
getY Nothing = Nothing

type Board = [Maybe Player]

data GameState = GameState
  { boardState' :: Board,
    currentPlayer' :: Player,
    players' :: [Player],
    end' :: Bool,
    message' :: Maybe String
  }

-- | Create an empty board
--
-- Examples:
-- >>> createEmptyBoard 3
-- [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
createEmptyBoard :: Int -> Board
createEmptyBoard sizeOfBoard = replicate (sizeOfBoard * sizeOfBoard) Nothing

-- | Get size of the board
-- Examples:
--
-- >>> getBoardSize [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
-- 3
getBoardSize :: Board -> Int
getBoardSize board = round (sqrt (fromIntegral (length board)))

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
-- >>> placePlayersMark [Nothing, Just (Player 'X')] 0 (Player 'O')
-- Just [Just (Player 'O'),Just (Player 'X')]
--
-- >>> placePlayersMark [Nothing, Just (Player 'X')] 1 (Player 'O')
-- Nothing
placePlayersMark :: Board -> Int -> Player -> Maybe Board
placePlayersMark board index player
  | isNothing (board !! index) = Just (take index board ++ [Just player] ++ drop (index + 1) board)
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
  index <- translateCoords move (getBoardSize board)
  placePlayersMark board index player

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

-- | Find a respective corner that serves as a start of specific sequence
--
-- Examples:
-- >>> findCorner 4 3 Diagonal
-- 0
-- >>> findCorner 2 3 Horizontal
-- 0
-- >>> findCorner 4 3 Vertical
-- 1
-- >>> findCorner 6 3 InverseDiagonal
-- 2
findCorner :: Int -> Int -> Direction -> Int
findCorner current size Horizontal
  | getY (translateIndex current size) == Just 1 = current
  | otherwise = findCorner (current - 1) size Horizontal
findCorner current size Vertical
  | getX (translateIndex current size) == Just 1 = current
  | otherwise = findCorner (current - size) size Vertical
findCorner current size Diagonal
  | getX (translateIndex current size) == Just 1 || getY (translateIndex current size) == Just 1 = current
  | otherwise = findCorner (current - size - 1) size Diagonal
findCorner current size InverseDiagonal
  | getY (translateIndex current size) == Just 3 || getX (translateIndex current size) == Just 1 = current
  | otherwise = findCorner (current - size + 1) size InverseDiagonal

-- | Get positions of player given a board and a player that we are looking for
--
-- Examples:
-- >>> getPlayerPositions [Nothing,Just (Player 'X'),Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing] (Player 'X')
-- [1]
getPlayerPositions :: Board -> Player -> [Int]
getPlayerPositions board player = elemIndices (Just player) board

-- | Checks if all the elements of one list are contained in another
--
-- Examples:
-- >>> elementsAreContainedIn [1,2,4] [1,2]
-- False
-- >>> elementsAreContainedIn [5,1] [1,5,8]
-- True
elementsAreContainedIn :: [Int] -> [Int] -> Bool
elementsAreContainedIn [] _ = True
elementsAreContainedIn sub list
  | head sub `elem` list = elementsAreContainedIn (tail sub) list
  | otherwise = False

-- | Assessing if player is a winner
--
-- Examples:
-- >>> isWinner 1 (Player 'X') [(Just (Player 'X')), (Just (Player 'X')), (Just (Player 'X')), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
-- True
-- >>> isWinner 1 (Player 'X') [Nothing, (Just (Player 'X')), (Just (Player 'X')), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
-- False
isWinner :: Int -> Player -> Board -> Bool
isWinner moveIndex player board =
  any
    ( \direction ->
        do
          let boardSize = getBoardSize board
          elementsAreContainedIn
            ( sequenceGenerator
                [findCorner moveIndex boardSize direction]
                boardSize
                direction
            )
            (getPlayerPositions board player)
    )
    [Horizontal, Vertical, Diagonal, InverseDiagonal]

-- | Convert Maybe to number
--
-- Examples:
--
-- >>> numifyMaybe (Just 5)
-- 5
--
-- >>> numifyMaybe Nothing
-- -1
numifyMaybe :: Maybe Int -> Int
numifyMaybe Nothing = -1
numifyMaybe (Just a) = a

-- | Switch players to the next one, supports more than 2
--
-- Examples:
--
-- >>> switchPlayers [(Player 'X'), (Player 'O')] (Player 'O')
-- Player 'X'
-- >>> switchPlayers [(Player 'X'), (Player 'O')] (Player 'X')
-- Player 'O'
switchPlayers :: [Player] -> Player -> Player
switchPlayers players currentPlayer = players !! mod (numifyMaybe (elemIndex currentPlayer players) + 1) (length players)

-- createRandomMove :: Float -> Board -> Move
-- createRandomMove seed board = do
--   let emptySquares = elemIndices Nothing board
--   emptySquares !! (seed - 1 / length emptySquares - 1)
