module Main where

import TicTacToe

main :: IO ()
main = do
  let emptyBoard = createEmptyBoard
  print emptyBoard
