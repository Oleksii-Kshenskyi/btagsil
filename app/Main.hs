module Main (main) where

import Lib
import World(initWorld)

main :: IO ()
main = run initWorld
