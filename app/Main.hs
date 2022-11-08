module Main (main) where

import Lib
import GameData(initWorld)

main :: IO ()
main = run initWorld
