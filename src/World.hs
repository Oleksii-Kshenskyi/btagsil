module World (
    whereAmI
) where

import qualified GameData as GD
import GameData (Location (..), World (..), Player (..))

import Data.Text (Text)
import Data.Map as M

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