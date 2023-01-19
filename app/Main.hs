module Main where

-- import System.Random

import TicTacToe

main :: IO ()
main = do
  let newGame = initializeGame 3
  _ <- playRounds newGame
  return ()

playRounds :: GameState -> IO GameState
playRounds gameState = do
  print (message' gameState)

  if end' gameState
    then return gameState
    else do
      putStrLn ("Current player: " ++ reprPlayer (currentPlayer' gameState))
      print (boardState' gameState)
      putStrLn "Enter row: "
      x <- getLine
      putStrLn "Enter column: "
      y <- getLine
      putStrLn ""
      playRounds (playOutRound gameState (Move (read x) (read y)))

-- g <- getStdGen
-- print (randomRs (1, 10) g :: [Int])
