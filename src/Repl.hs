{-# LANGUAGE OverloadedStrings #-}

module Repl (
    chooseAction,
    instructRepl,
    Action (..),
    ControlFlow (..)
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map as M

import Actions
import GameData (World (..), Location(..), Player(..))
import qualified GameData as GD

data ControlFlow = EmptyResponse
                 | TextResponse Text
                 | ExitGame Text

describeLoc :: Location -> Text
describeLoc loc = GD.describeLoc (lname loc) (ldescription loc)

getCurLoc :: World -> Location
getCurLoc world = case M.lookup curLocName locs of
                      Nothing -> error "getCurLoc: UNREACHABLE: Current location doesn't exist?!"
                      Just loc -> loc
    where curLocName = currentLocation $ player world
          locs = locations world

whereAmI :: World -> Text
whereAmI world = describeLoc curLoc
    where curLoc = getCurLoc world

performWhere :: World -> [Text] -> (World, Action)
performWhere world what = case what of
                            [] -> (world, Echo GD.whereWhat)
                            ["is"] -> (world, Echo GD.whereIsWhat)
                            "is" : obj -> (world, Echo $ GD.noClueWhereIsObject $ T.unwords obj)
                            ["am", "i"] -> (world, Echo $ whereAmI world)
                            _ -> (world, Echo GD.wrongWhere)

instructRepl :: Action -> ControlFlow
instructRepl action =
    case action of
        Echo echoed -> TextResponse echoed
        Unknown what -> TextResponse $ unknownMessage what
        Exit -> ExitGame exitMessage
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
