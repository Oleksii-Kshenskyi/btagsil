module Main (main) where

import Lib (run)
import World(initWorld)

main :: IO ()
main = run initWorld
