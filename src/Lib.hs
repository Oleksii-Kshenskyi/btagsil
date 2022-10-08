module Lib
    ( someFunc
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as I

someFunc :: IO ()
someFunc = I.putStr (T.map (\c -> if c == '.' then '!' else c) (T.pack "KEKW. Totally KEKW. Ahaha :D\n"))
