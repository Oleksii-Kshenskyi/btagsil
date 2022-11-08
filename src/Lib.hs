{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where

import Data.Text
import Data.Text.IO as TI
import qualified System.IO as SI
import qualified System.Exit as E

import Repl
import GameData (World)
import qualified GameData as GD

act :: Action -> IO ()
act Empty = TI.putStr ""
act (Exit message) = TI.putStrLn (message <> "\n") >> E.exitSuccess
act (Echo response) = TI.putStrLn (response <> "\n")
act (Unknown thing) = TI.putStrLn $ GD.unknownMessage thing <> "\n"

readOnce :: IO Text
readOnce = TI.putStr ">> " *> SI.hFlush SI.stdout *> TI.getLine

run :: World -> IO ()
run world = do
    (newWorld, newAction) <- chooseAction world readOnce
    act newAction *> run newWorld
