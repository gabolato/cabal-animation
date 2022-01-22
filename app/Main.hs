module Main where

import Lib(createBoard, Vector, animate)

main :: IO ()
main = animate (createBoard 10 10)