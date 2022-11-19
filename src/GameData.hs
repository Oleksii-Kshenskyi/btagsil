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

data BehaviorNode =
    IntNode Int
    | TextNode Text
    | LinesNode [Text]
    | WeaponsNode (M.Map Text Weapon)
    deriving (Show)

data Player = Player {
    _currentLocation :: Text,
    _weapon :: Weapon
} deriving (Show)
$(makeLenses ''Player)

data Object = Object {
    _oname :: Text,
    _odescription :: Text,
    _properties :: [Text],
    _behavior :: M.Map Text BehaviorNode
} deriving (Show)
$(makeLenses ''Object)

data Location = Location {
    _lname :: Text,
    _ldescription :: Text,
    _connected :: [Text],
    _objects :: M.Map Text Object
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

initAxe :: Weapon
initAxe = Weapon {
    _wname = "axe",
    _wdescription = "a humongous razor-sharp double-headed greataxe"
}

initSword :: Weapon
initSword = Weapon {
    _wname = "sword",
    _wdescription = "a gorgeous long ornamented claymore"
}

initBow :: Weapon
initBow = Weapon {
    _wname = "bow",
    _wdescription = "a gigantic greatbow that uses spears as arrows"
}

initPlayer :: Player
initPlayer = Player {
    _currentLocation = "forest",
    _weapon = initFists
}

initGuardBox :: [Text]
initGuardBox = [
        "How's your day going today, Your Highness?",
        "Did you know that dragons can fly?",
        "I've been thinking about the purpose of my life a lot recently... Why am I here?",
        "I want to kiss a girl so much...",
        "I'm a bit under the weather today, apologies.",
        "You look lovely today, Your Highness!",
        "WOW! The hat REALLY suits you! So stylish!",
        "It's important to remember to brush your teeth every morning.",
        "A bottle of fine ale would hit the spot right about now...",
        "It's impressive how quickly these tourists litter the square. Ugh.",
        "Did you know there's a fine weapon shop just nearby? Try going there!",
        "Are you tired?",
        "I remember that time I was a wee little lad..."
    ]

initGuard :: Object 
initGuard = Object {
    _oname = "guard",
    _odescription = "a big burly man in heavy armor wielding a halberd.\nHe seems friendly and doesn't seem to mind chatting with people who visit the square.",
    _properties = ["talks"],
    _behavior = M.fromList [("pandora's box", LinesNode initGuardBox)]
}

initShopkeeper :: Object
initShopkeeper = Object {
    _oname = "shopkeeper",
    _odescription = "an old man with an eye for trade.\nHe's throwing glances at you hoping you'll buy something from him",
    _properties = ["talks", "sells"],
    _behavior = M.fromList [("sells", WeaponsNode (M.fromList [("axe", initAxe),
                                                               ("sword", initSword),
                                                               ("bow", initBow)]))]
}

initForest :: Location
initForest = Location {
    _lname = "forest",
    _ldescription = "a beautiful rainforest.\nThere's a fresh smell of nature after rain in the air.\nSunshine is filtering through the tall trees, creating a beautiful and peaceful feeling inside",
    _connected = ["square"],
    _objects = M.empty
}

initSquare :: Location
initSquare = Location {
    _lname = "square",
    _ldescription = "a gigantic square full of people.\nYou suddenly long for some adventure!",
    _connected = ["forest", "weapon shop"],
    _objects = M.fromList [("guard", initGuard)]
}

initWeaponShop :: Location
initWeaponShop = Location {
    _lname = "weapon shop",
    _ldescription = "crammed with fine quality weapons to buy.\nAn axe, a sword and a bow catch your eye.",
    _connected = ["square"],
    _objects = M.fromList [("shopkeeper", initShopkeeper)]
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

youSee :: Text -> Text
youSee stuff = "You see " <> stuff <> " here."

youSeeObject :: Text -> Text -> Text
youSeeObject name descr = "You see the " <> name <> ". It's " <> descr <> "."

youSeeWeapon :: Text -> Text
youSeeWeapon weaponDescr = "You see " <> weaponDescr <> "."

shopkeeperLine :: Text
shopkeeperLine = "Stop talking and buy something already, you flirtatious vagabond!"

entitySays :: Text -> Text -> Text
entitySays entityName says = "The " <> entityName <> " says: '" <> says <> "'"

thanksForBuying :: Text -> Text -> Text
thanksForBuying sellerName thing = "The " <> sellerName <> " says: 'Thanks for buying the " <> thing <> "!"

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

-- Look errors

wrongLook :: Text
wrongLook = "I see. You want to look. But how and where?\nMaybe try 'look at <object>' or 'look around'?"

lookWhere :: Text
lookWhere = "Look... where?\nMaybe try 'look at <object>' or 'look around'"

lookAtWhat :: Text
lookAtWhat = "Look at... What?"

nothingToSeeHere :: Text
nothingToSeeHere = "You don't see anything of importance around here."

noObjAround :: Text -> Text
noObjAround objName = "You don't see any " <> objName <> "s around here."

-- Talk errors

okImTalking :: Text
okImTalking = "Talk? OK, I'm talking. And?\nMaybe try 'talk to <object>'?\nTry 'look around' to see who you can talk to."

talkToWho :: Text
talkToWho = "Talk to... Who?"

wrongTalk :: Text
wrongTalk = "Umm... Talk?.. How? What?!\nMaybe try 'talk to <object>'?"


objDoesntTalk :: Text -> Text
objDoesntTalk objName = "The " <> objName <> " doesn't seem too interested in talking to you."

noObjToTalkTo :: Text -> Text
noObjToTalkTo objName = "You don't see any " <> objName <> "s to talk to."

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

-- Buy errors

buyFromSomeone :: Text
buyFromSomeone = "You can only buy things 'from' someone.\nTry 'buy <thing> from <seller>'."

buyWhat :: Text
buyWhat = "Buy what?\nTry 'buy <thing> from <seller>'."

buyThingFromWho :: Text -> Text
buyThingFromWho thing = "Buy " <> thing <> " from... who?"

sellerDoesntExist :: Text -> Text
sellerDoesntExist sellerName = "You don't see any " <> sellerName <> "s around that would be willing to sell you stuff."

sellerDoesntSell :: Text -> Text
sellerDoesntSell sellerName = "The " <> sellerName <> " does not seem interested in selling you stuff."

sellerDoesntSellThing :: Text -> Text -> Text
sellerDoesntSellThing sellerName thing = "The " <> sellerName <> " doesn't have any " <> thing <> "s to sell you."