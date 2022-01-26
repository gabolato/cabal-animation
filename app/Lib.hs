module Lib
    (
        -- Board(..),
        -- State(..),
        createBoard,
        animate,
        Vector,
    ) where

import Control.Concurrent (threadDelay)
import Data.Array
import System.Console.ANSI

-- TYPES!
type Vector = (Int, Int)
type Row  = [Bool] 

-- Board
-- slots are each cell with a Bool
-- position vector will be something like (x, y)
data Board = Board { slots :: [Row], position :: Vector }
  deriving (Show)

-- a Matrix that represents the map. False is "0" and True is "X"
-- n: amount of rows
-- m: amount of columns
createBoard :: Int -> Int -> Board
createBoard n m = Board { slots = replicate n (replicate m False), position = (0, 0) }

changeRow :: Row -> Int -> Row
changeRow row pos = 
  let height = length row
  in [if y == pos then not (row !! y) else row !! y  | y <- [0 .. height -1 ]] -- TODO implement this

changeSlot :: [Row] -> Vector -> [Row]
changeSlot slots (posX, posY) = 
  let width = length slots
      height = length (head slots)
  in [if x == posX then changeRow (slots !! x) posY else slots !! x  | x <- [0 .. width -1 ]]

next :: Board -> Board
next (Board slots position) =
  let x = fst position
      y = snd position
      lineBreak = x >= length (head slots)
      nextX = if not lineBreak then x + 1 else 0
      columnOverflow = y >= length (head slots)
      -- If we iterate over all the board we start again
      nextY = if lineBreak 
        then if columnOverflow then 0 else y + 1 
        else y
      newValue = not ((slots !! x) !! y)
      -- change boolean status
      newSlots = changeSlot slots (x, y)
      -- slots = [ drawRow (slots !! row) | row <- [0 .. (length (head slots) - 1)] ]
  in Board newSlots (nextX, nextY)

--------------------------------------------------------------------------------
-- Draw
drawBoard :: Board -> String
drawBoard (Board slots _) =
  unlines $ reverse $ [ drawRow (slots !! row) | row <- [0 .. (length (head slots) - 1)] ]

drawRow :: Row -> String
drawRow row = [charAt (row !! col) | col <- [0 .. (length row - 1)]]

charAt :: Bool -> Char
charAt m = if not m
    then 'x'
    else '#'

--------------------------------------------------------------------------------
-- Animate!

-- Refresh rate of 1 second.
animate :: Board -> IO ()
-- animate board = putStr (drawBoard board) >> threadDelay 1000000 >> animate (next board)
animate board = putStr (drawBoard board) >> threadDelay 1000000 >> clearScreen >> animate (next board)
