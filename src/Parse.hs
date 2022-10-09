module Parse (
    parse,
    Action (..)
) where

import qualified Data.Text as T

data Action =
    Exit
    | Empty
    | Echo T.Text
    | Unknown T.Text
    deriving (Show)

parse :: T.Text -> Action
parse input | len == 0 = Empty
            | (head tags == T.pack "exit") && len == 1 = Exit
            | (head tags == T.pack "echo") && len > 1 = Echo arg
            | otherwise = Unknown (T.unwords tags)
    where
        tags = T.words input
        len = length tags
        arg = T.unwords $ drop 1 $ tags

