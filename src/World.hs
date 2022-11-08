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
import Control.Lens
import Control.Arrow ((>>>))

describeLoc :: Location -> Text
describeLoc loc = GD.describeLoc (loc ^.to lname) (loc ^.to ldescription)

getCurLoc :: World -> Location
getCurLoc world = case locs ^.at curLocName  of
                      Nothing -> error "getCurLoc: UNREACHABLE: Current location doesn't exist?!"
                      Just loc -> loc
    where curLocName = world ^.to (player >>> currentLocation)
          locs = world ^.to locations

articledEnumeration :: [Text] -> Text -> Text
articledEnumeration locs article = T.intercalate ", " $ P.map (\s -> article <> " " <> s) locs

-- Exported (public) functionality used by Repl.hs

whereAmI :: World -> Text
whereAmI world = describeLoc curLoc
    where curLoc = getCurLoc world

possibleDestinations :: World -> Text
possibleDestinations world = GD.youCanGoTo destinations
    where curLoc = getCurLoc world
          connLocs = curLoc ^.to connected
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