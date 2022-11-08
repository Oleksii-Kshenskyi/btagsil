{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Data structures

module GameData (
    module GameData,
) where

import qualified Data.Map as M
import Data.Text (Text)
import Control.Lens (makeLenses)
import Control.Lens.TH ( makeFields)

data Weapon = Weapon {
    wname :: Text,
    wdescription :: Text
} deriving (Show)
$(makeLenses ''Weapon)
$(makeFields ''Weapon)

data Player = Player {
    currentLocation :: Text,
    weapon :: Weapon
} deriving (Show)
$(makeLenses ''Player)
$(makeFields ''Player)

data Location = Location {
    lname :: Text,
    ldescription :: Text,
    connected :: [Text]
} deriving (Show)
$(makeLenses ''Location)
$(makeFields ''Location)

data World = World {
    player :: Player,
    locations :: M.Map Text Location 
} deriving (Show)
$(makeLenses ''World)
$(makeFields ''World)

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
    ldescription = "a beautiful rainforest.\nThere's a fresh smell of nature after rain in the air.\nSunshine is filtering through the tall trees, creating a beautiful and peaceful feeling inside",
    connected = ["square"]
}

initSquare :: Location
initSquare = Location {
    lname = "square",
    ldescription = "a gigantic square full of people.\nYou suddenly long for some adventure!",
    connected = ["forest"]
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