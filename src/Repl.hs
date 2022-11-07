{-# LANGUAGE OverloadedStrings #-}

module Repl (
    chooseAction,
    instructRepl,
    Action (..),
    ControlFlow (..)
) where

import Data.Text (Text)
import qualified Data.Text as T

import Actions

data ControlFlow = EmptyResponse
                 | TextResponse Text
                 | ExitGame Text

instructRepl :: Action -> ControlFlow
instructRepl action =
    case action of
        Echo echoed -> TextResponse echoed
        Unknown what -> TextResponse $ unknownMessage what
        Exit -> ExitGame exitMessage
        Empty -> EmptyResponse

chooseAction :: Text -> Action
chooseAction input = case tags of [] -> Empty
                                  ["exit"] -> Exit
                                  "echo" : arg -> Echo (T.unwords arg)
                                  _ -> Unknown input
    where tags = T.words input
