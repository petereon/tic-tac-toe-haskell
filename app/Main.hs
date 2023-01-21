module Main where

-- import System.Random

import TicTacToe

main :: IO ()
main = do
  putStrLn "Provide board size:"
  boardSize <- getLine
  putStrLn "Provide number of players:"
  numOfPlayers <- getLine
  let newGame = initializeGame (read boardSize) (read numOfPlayers)
  _ <- playRounds newGame
  return ()

playRounds :: GameState -> IO GameState
playRounds gameState = do
  putStrLn (reprBoard (boardState' gameState))
  putStrLn (reprMessage (message' gameState))

  if end' gameState
    then return gameState
    else do
      putStrLn ("Current player: " ++ reprPlayer (currentPlayer' gameState))

      putStrLn "Enter row: "
      x <- getLine
      putStrLn "Enter column: "
      y <- getLine
      putStrLn ""
      playRounds (playOutRound gameState (Move (read x) (read y)))
