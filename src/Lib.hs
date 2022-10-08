module Lib
    ( run
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as I
import qualified System.IO as SI
import qualified System.IO.Unsafe as U
import qualified Control.Monad as C

echo :: T.Text -> IO ()
echo x = I.putStrLn (x <> T.pack "\n")

readOnce :: T.Text
readOnce = U.unsafePerformIO $ I.putStr (T.pack ">> ") >> SI.hFlush SI.stdout >> I.getLine

run :: IO ()
run = do 
    let input = readOnce
    C.unless (input == T.pack "exit") $ echo input >> run
