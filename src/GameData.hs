{-# LANGUAGE OverloadedStrings #-}

-- Data structures

module GameData (
    module GameData,
) where

import qualified Data.Map as M
import Data.Text (Text)

data Weapon = Weapon {
    wname :: Text,
    wdescription :: Text
}

data Player = Player {
    currentLocation :: Text,
    weapon :: Weapon
}

data Location = Location {
    lname :: Text,
    ldescription :: Text
}

data World = World {
    player :: Player,
    locations :: M.Map Text Location 
}

initFists :: Weapon
initFists = Weapon {
    wname = "fists",
    wdescription = "just your bare fists"
}

initPlayer :: Player
initPlayer = Player {
    currentLocation = "forest",
    weapon = initFists
}

initForest :: Location
initForest = Location {
    lname = "forest",
    ldescription = "a beautiful rainforest.\nThere's a fresh smell of nature after rain in the air.\nSunshine is filtering through the tall trees, creating a beautiful and peaceful feeling inside"
}

initWorld :: World
initWorld = World {
    player = initPlayer,
    locations = M.fromList [("forest", initForest)]
}

-- System messages

exitMessage :: Text
exitMessage = "Thanks for playing!"

unknownMessage :: Text -> Text
unknownMessage what = "I'm sorry, I don't know what '" <> what <> "' is."

-- Navigational messages

describeLoc :: Text -> Text -> Text
describeLoc locName locDescr = "You're in the " <> locName <> ".\nIt's " <> locDescr <> "."

-- Error messages

-- Where errors

whereWhat :: Text
whereWhat = "Where... what?"

whereIsWhat :: Text
whereIsWhat = "Where is... what?"

noClueWhereIsObject :: Text -> Text
noClueWhereIsObject obj = "No clue where '" <> obj <> "' is ¯\\_(ツ)_/¯"

wrongWhere :: Text
wrongWhere = "Where... ugh... What do you mean?\nMaybe you meant 'where is <object>' or 'where can i go'\nOr maybe 'where am i'?"