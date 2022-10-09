module Lib
    ( run
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as I
import qualified System.IO as SI
import qualified System.Exit as E
import qualified System.IO.Unsafe as U
import qualified Control.Monad as C
import Parse
import Actions

echo :: Maybe T.Text -> IO ()
echo (Just x) = I.putStrLn (x <> T.pack "\n")
echo Nothing = putStr ""

strResponse :: Maybe T.Text -> T.Text
strResponse (Just t) = t
strResponse Nothing = T.pack ""

readOnce :: T.Text
readOnce = U.unsafePerformIO $ I.putStr (T.pack ">> ") >> SI.hFlush SI.stdout >> I.getLine

exitGame :: IO ()
exitGame = (I.putStrLn $ exitMessage <> T.pack "\n") >> E.exitWith E.ExitSuccess

run :: IO ()
run = if text == exitMessage then exitGame else (echo $ execute $ parse readOnce) >> run
    where
        response = execute $ parse readOnce
        text = strResponse response
