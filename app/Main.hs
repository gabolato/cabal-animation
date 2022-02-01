module Main where

import Lib(Vector, createBoard, animate)

main :: IO ()
main = animate (createBoard 10 5)