{-# LANGUAGE OverloadedStrings #-}

module Actions (
    exitMessage,
    unknownMessage,
    Action (..)
) where

import Data.Text

data Action =
    Exit
    | Empty
    | Echo Text
    | Unknown Text
    deriving (Show)


exitMessage :: Text
exitMessage = "Thanks for playing!"

unknownMessage :: Text -> Text
unknownMessage what = "I'm sorry, I don't know what '" <> what <> "' is."
