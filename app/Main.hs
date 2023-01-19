module Main where

-- import System.Random

import TicTacToe

main :: IO ()
main = do
  print "jajaja"

playRounds :: IO GameState -> IO GameState
playRounds gameState' = do
  -- Here I need to print the board along with some messages, going to need a function that prints a board nicely
  print "Enter row"
  x <- getLine
  print "Enter column"
  y <- getLine
  gameState <- gameState'

-- g <- getStdGen
-- print (randomRs (1, 10) g :: [Int])
