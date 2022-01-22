module Lib
    (
        -- Env(..),
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
type Board  = [Row] 

-- direction vector will be something like (x, y) where x and y can be 0, 1, -1
data State = State { position :: Vector, direction :: Vector } -- speed :: Int }
  deriving (Show)

-- a Matrix that represents the map. False is "0" and True is "X"
-- n: amount of rows
-- m: amount of columns
createBoard :: Int -> Int -> Board
createBoard n m = replicate n (replicate m False)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- State
-- next :: Env -> State -> State
-- next (Env (width, height)) (State (posX, posY) (dirX, dirY)) =
--   let posX' = posX + dirX
--       posY' = posY + dirY
--       hasCrossedTopEdge = posY' > height
--       hasCrossedBottomEdge = posY' < 0
--       hasCrossedLeftEdge = posX' < 0
--       hasCrossedRightEdge = posX' > width
--       dirXFinal = if hasCrossedLeftEdge || hasCrossedRightEdge then (-dirX) else dirX
--       dirYFinal = if hasCrossedTopEdge || hasCrossedBottomEdge then (-dirY) else dirY
--       posXFinal = if hasCrossedLeftEdge || hasCrossedRightEdge
--                     then posX + dirXFinal
--                     else posX'
--       posYFinal = if hasCrossedTopEdge || hasCrossedBottomEdge
--                     then posY + dirYFinal
--                     else posY'
--   in State {
--     position = (posXFinal, posYFinal),
--     direction = (dirXFinal, dirYFinal)
--   }

--------------------------------------------------------------------------------
-- Draw
drawState :: Board -> String
drawState board =
  unlines $ [ drawRow (board !! row) | row <- [0 .. (length (head board) - 1)] ]

drawRow :: Row -> String
drawRow row = [charAt (row !! col) | col <- [0 .. (length row - 1)]]

charAt :: Bool -> Char
charAt m = if not m
    then ' '
    else '#'

--------------------------------------------------------------------------------
-- Animate!

-- Refresh rate of 1 second.
animate :: Board -> IO ()
animate board = putStr (drawState board) -- >> threadDelay 1000000 >> clearScreen >> animate board (next board vector)
