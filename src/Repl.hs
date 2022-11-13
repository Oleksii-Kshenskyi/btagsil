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

-- Navigational and informational actions that don't mutate the world

performLook :: World -> [Text] -> (World, Action)
performLook world direction = case direction of
                                [] -> (world, Echo GD.lookWhere)
                                ["at"] -> (world, Echo GD.lookAtWhat)
                                "at" : obj -> (world, Echo $ W.lookAtObject world obj)
                                ["around"] -> (world, Echo $ W.lookAround world)
                                _ -> (world, Echo GD.wrongLook)

performWhere :: World -> [Text] -> (World, Action)
performWhere world what = case what of
                            [] -> (world, Echo GD.whereWhat)
                            ["is"] -> (world, Echo GD.whereIsWhat)
                            "is" : obj -> (world, Echo $ GD.noClueWhereIsObject $ T.unwords obj)
                            ["am", "i"] -> (world, Echo $ W.whereAmI world)
                            ["can", "i", "go"] -> (world, Echo $ W.possibleDestinations world)
                            _ -> (world, Echo GD.wrongWhere)

performTalk :: World -> [Text] -> (World, Action)
performTalk world obj = case obj of
                            [] -> (world, Echo GD.okImTalking)
                            ["to"] -> (world, Echo GD.talkToWho)
                            "to" : target -> (world, Echo $ W.talkToObject world target)
                            _ -> (world, Echo GD.wrongTalk)

-- Actions that mutate the world

performGo :: World -> [Text] -> (World, Action)
performGo world dest = case dest of
                        [] -> (world, Echo GD.goWhere)
                        ["to"] -> (world, Echo GD.goToWhere)
                        "to" : place -> W.goTo world place
                        _ -> (world, Echo GD.tryGoingToPlaces)

-- REPL's main entry point for choosing the specific action to execute

chooseAction :: World -> IO Text -> IO (World, Action)
chooseAction world inputIOed = do
    textPls <- inputIOed
    return $ case T.words textPls of 
                [] -> (world, Empty)
                ["exit"] -> (world, Exit GD.exitMessage)
                "echo" : arg -> (world, Echo (T.unwords arg))
                "where" : what -> performWhere world what
                "look" : direction -> performLook world direction
                "go" : loc -> performGo world loc
                "talk" : obj -> performTalk world obj
                _ -> (world, Unknown textPls)
