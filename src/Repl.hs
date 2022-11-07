{-# LANGUAGE OverloadedStrings #-}

module Repl (
    chooseAction,
    Action (..)
) where

import qualified Data.Text as T

import Actions

chooseAction :: T.Text -> Action
chooseAction input = case tags of [] -> Empty
                                  ["exit"] -> Exit
                                  "echo" : arg -> Echo (T.unwords arg)
                                  _ -> Unknown input
    where tags = T.words input
