{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Data structures

module GameData (
    module GameData,
) where

import qualified Data.Map as M
import Data.Text (Text)
import Control.Lens (makeLenses)

data Action =
    Exit Text
    | Empty
    | Echo Text
    | Unknown Text
    deriving (Show)

data Weapon = Weapon {
    _wname :: Text,
    _wdescription :: Text
} deriving (Show)
$(makeLenses ''Weapon)

data Player = Player {
    _currentLocation :: Text,
    _weapon :: Weapon
} deriving (Show)
$(makeLenses ''Player)

data Location = Location {
    _lname :: Text,
    _ldescription :: Text,
    _connected :: [Text]
} deriving (Show)
$(makeLenses ''Location)

data World = World {
    _player :: Player,
    _locations :: M.Map Text Location
} deriving (Show)
$(makeLenses ''World)


initFists :: Weapon
initFists = Weapon {
    _wname = "fists",
    _wdescription = "just your bare fists"
}

initPlayer :: Player
initPlayer = Player {
    _currentLocation = "forest",
    _weapon = initFists
}

initForest :: Location
initForest = Location {
    _lname = "forest",
    _ldescription = "a beautiful rainforest.\nThere's a fresh smell of nature after rain in the air.\nSunshine is filtering through the tall trees, creating a beautiful and peaceful feeling inside",
    _connected = ["square"]
}

initSquare :: Location
initSquare = Location {
    _lname = "square",
    _ldescription = "a gigantic square full of people.\nYou suddenly long for some adventure!",
    _connected = ["forest"]
}

-- System messages

exitMessage :: Text
exitMessage = "Thanks for playing!"

unknownMessage :: Text -> Text
unknownMessage what = "I'm sorry, I don't know what '" <> what <> "' is."

-- Navigational messages

describeLoc :: Text -> Text -> Text
describeLoc locName locDescr = "You're in the " <> locName <> ".\nIt's " <> locDescr <> "."

youCanGoTo :: Text -> Text
youCanGoTo these = "You can go to " <> these <> " from here."

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

-- Go errors

goWhere :: Text
goWhere = "Go... where?"

goToWhere :: Text
goToWhere = "Go to... where?"

tryGoingToPlaces :: Text
tryGoingToPlaces = "Where are you trying to go?\nTry 'go to <place>' to go somewhere.\nMaybe try 'where can i go' to see where you can get from here?"

noSuchLoc :: Text -> Text
noSuchLoc place = "You don't see any paths leading to a " <> place <> " anywhere around here."

placeNotConnected :: Text -> Text -> Text
placeNotConnected curLoc place = "You can't move from " <> curLoc <> " to " <> place <> ", they're not connected!\nTry 'where can i go' to se where you can go from here."

alreadyThere :: Text -> Text
alreadyThere place = "You cannot go to the " <> place <> ", you're already there!"

youWentTo :: Text -> Text
youWentTo place = "You went to " <> place <> "."
