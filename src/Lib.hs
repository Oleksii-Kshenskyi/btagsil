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

act :: ControlFlow -> IO ()
act EmptyResponse = TI.putStr ""
act (ExitGame message) = TI.putStrLn (message <> "\n") >> E.exitSuccess
act (TextResponse response) = TI.putStrLn (response <> "\n")

readOnce :: IO Text
readOnce = TI.putStr ">> " *> SI.hFlush SI.stdout *> TI.getLine

run :: World -> IO ()
run world = do
    (newWorld, newAction) <- chooseAction world readOnce
    act (instructRepl newAction) *> run newWorld
