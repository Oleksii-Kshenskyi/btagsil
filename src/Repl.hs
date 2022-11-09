{-# LANGUAGE OverloadedStrings #-}

module Repl (
    chooseAction,
    Action (..)
) where

import Data.Text (Text)
import qualified Data.Text as T

import GameData (World (..), Action (..))
import qualified GameData as GD
import qualified World as W

performWhere :: World -> [Text] -> (World, Action)
performWhere world what = case what of
                            [] -> (world, Echo GD.whereWhat)
                            ["is"] -> (world, Echo GD.whereIsWhat)
                            "is" : obj -> (world, Echo $ GD.noClueWhereIsObject $ T.unwords obj)
                            ["am", "i"] -> (world, Echo $ W.whereAmI world)
                            ["can", "i", "go"] -> (world, Echo $ W.possibleDestinations world)
                            _ -> (world, Echo GD.wrongWhere)

performGo :: World -> [Text] -> (World, Action)
performGo world dest = case dest of
                        [] -> (world, Echo GD.goWhere)
                        ["to"] -> (world, Echo GD.goToWhere)
                        "to" : place -> W.goTo world place
                        _ -> (world, Echo GD.tryGoingToPlaces)

chooseAction :: World -> IO Text -> IO (World, Action)
chooseAction world inputIOed = do
    textPls <- inputIOed
    return $ case T.words textPls of 
                [] -> (world, Empty)
                ["exit"] -> (world, Exit GD.exitMessage)
                "echo" : arg -> (world, Echo (T.unwords arg))
                "where" : what -> performWhere world what
                "go" : loc -> performGo world loc
                _ -> (world, Unknown textPls)
