module Main where

import Lib(Env(..), State(..), animate)

main :: IO ()
main = animate (Env (10, 10)) (State (9, 10) (1, 1))
