module Lib
    ( run
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as I
import qualified System.IO as SI
import qualified System.Exit as E
import qualified System.IO.Unsafe as U

import Parse
import Actions

act :: ControlFlow -> IO ()
act EmptyResponse = I.putStr $ T.pack ""
act (ExitGame message) = (I.putStrLn $ message <> T.pack "\n") >> E.exitWith E.ExitSuccess
act (TextResponse response) = I.putStrLn (response <> T.pack "\n")

readOnce :: T.Text
readOnce = U.unsafePerformIO $ I.putStr (T.pack ">> ") >> SI.hFlush SI.stdout >> I.getLine

run :: IO ()
run = (act $ execute $ parse readOnce) >> run
