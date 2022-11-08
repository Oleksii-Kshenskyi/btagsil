{-# LANGUAGE OverloadedStrings #-}

module Repl (
    chooseAction,
    instructRepl,
    Action (..),
    ControlFlow (..)
) where

import Data.Text (Text)
import qualified Data.Text as T

import GameData (World (..))
import qualified GameData as GD
import qualified World as W

data ControlFlow = EmptyResponse
                 | TextResponse Text
                 | ExitGame Text

data Action =
    Exit
    | Empty
    | Echo Text
    | Unknown Text
    deriving (Show)

performWhere :: World -> [Text] -> (World, Action)
performWhere world what = case what of
                            [] -> (world, Echo GD.whereWhat)
                            ["is"] -> (world, Echo GD.whereIsWhat)
                            "is" : obj -> (world, Echo $ GD.noClueWhereIsObject $ T.unwords obj)
                            ["am", "i"] -> (world, Echo $ W.whereAmI world)
                            _ -> (world, Echo GD.wrongWhere)

instructRepl :: Action -> ControlFlow
instructRepl action =
    case action of
        Echo echoed -> TextResponse echoed
        Unknown what -> TextResponse $ GD.unknownMessage what
        Exit -> ExitGame GD.exitMessage
        Empty -> EmptyResponse

chooseAction :: World -> IO Text -> IO (World, Action)
chooseAction world inputIOed = do
    textPls <- inputIOed
    return $ case T.words textPls of 
                [] -> (world, Empty)
                ["exit"] -> (world, Exit)
                "echo" : arg -> (world, Echo (T.unwords arg))
                "where" : what -> performWhere world what
                _ -> (world, Unknown textPls)
