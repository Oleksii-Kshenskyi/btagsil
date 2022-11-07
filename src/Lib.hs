{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where

import Data.Text
import Data.Text.IO as TI
import qualified System.IO as SI
import qualified System.Exit as E
import qualified System.IO.Unsafe as U

import Repl

act :: ControlFlow -> IO ()
act EmptyResponse = TI.putStr ""
act (ExitGame message) = TI.putStrLn (message <> "\n") >> E.exitSuccess
act (TextResponse response) = TI.putStrLn (response <> "\n")

readOnce :: Text
readOnce = U.unsafePerformIO $ TI.putStr ">> " >> SI.hFlush SI.stdout >> TI.getLine

run :: IO ()
run = act (instructRepl $ chooseAction readOnce) >> run
