module Main where


data Move = Move Integer Integer

data Row = Row Integer Integer Integer
data Board = Board Row Row Row

createEmptyBoard :: Board
createEmptyBoard = Board (Row 0 0 0) (Row 0 0 0) (Row 0 0 0)

makeMove :: Move Board -> Board


main :: IO ()
main = haskellSay
    "Hello, Haskell!"
