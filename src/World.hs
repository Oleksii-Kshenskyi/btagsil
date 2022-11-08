{-# LANGUAGE OverloadedStrings #-}

module World (
    whereAmI, possibleDestinations, initWorld
) where

import qualified GameData as GD
import GameData (
    Location (..), World (..), Player (..),
    initPlayer, initForest, initSquare)

import Data.Text()
import Data.Map as M
import Prelude as P
import Data.Text as T

describeLoc :: Location -> Text
describeLoc loc = GD.describeLoc (lname loc) (ldescription loc)

getCurLoc :: World -> Location
getCurLoc world = case M.lookup curLocName locs of
                      Nothing -> error "getCurLoc: UNREACHABLE: Current location doesn't exist?!"
                      Just loc -> loc
    where curLocName = currentLocation $ player world
          locs = locations world

articledEnumeration :: [Text] -> Text -> Text
articledEnumeration locs article = T.intercalate ", " $ P.map (\s -> article <> " " <> s) locs

-- Exported (public) functionality used by Repl.hs

whereAmI :: World -> Text
whereAmI world = describeLoc curLoc
    where curLoc = getCurLoc world

possibleDestinations :: World -> Text
possibleDestinations world = GD.youCanGoTo destinations
    where curLoc = getCurLoc world
          connLocs = connected curLoc
          destinations = articledEnumeration connLocs "the"

-- Initializing the world

initWorld :: World
initWorld = World {
    player = initPlayer,
    locations = M.fromList [
        ("forest", initForest),
        ("square", initSquare)
    ]
}