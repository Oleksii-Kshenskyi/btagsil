{-# LANGUAGE OverloadedStrings #-}

module World (
    whereAmI, possibleDestinations, initWorld,
    goTo, lookAround, lookAtObject, talkToObject
) where

import qualified GameData as GD
import GameData

import Data.Text()
import Data.Map as M
import Prelude as P
import Data.Text as T
import Control.Lens

import Data.Maybe (isJust, fromJust)

describeLoc :: Location -> Text
describeLoc loc = GD.describeLoc (loc ^. lname) (loc ^. ldescription)

getCurLoc :: World -> Location
getCurLoc world = case locs ^.at curLocName  of
                      Nothing -> error "getCurLoc: UNREACHABLE: Current location doesn't exist?!"
                      Just loc -> loc
    where curLocName = world ^. (player . currentLocation)
          locs = world ^. locations

articledEnumeration :: [Text] -> Text -> Text
articledEnumeration locs article = T.intercalate ", " $ P.map (\s -> article <> " " <> s) locs

moveBetweenLocs :: World -> Text -> (World, Action)
moveBetweenLocs world place = (newWorld, Echo response)
    where newWorld = world & player . currentLocation .~ place
          response = GD.youWentTo place

takeFromPandorasBox :: [Text] -> Int -> Text
takeFromPandorasBox theBox randInt = theBox ^. ix randomIndex
    where randomIndex = randInt `mod` P.length theBox

talkToGuard :: Object -> Int -> Text
talkToGuard guard randInt = GD.entitySays guardName pandoraSays
    where guardName = guard ^. oname
          mBehaviorNode = guard ^. behavior . at "pandora's box"
          pandoraSays = case mBehaviorNode of
                            Nothing -> error "World: talkToGuard: guard's pandora's box node is a Nothing!"
                            Just node -> case node of
                                            LinesNode lns -> takeFromPandorasBox lns randInt
                                            _ -> error "World: talkToGuard: pandora's box is not a LinesNode!"
          

executeTalk :: World -> Text -> Int -> Text
executeTalk world objName randInt = case objName of
                                        "guard" -> talkToGuard (fromJust theObj) randInt
                                        wtf -> error "World: executeTalk: ERROR: don't know how to talk to " <> wtf <> "."
    where theObj = getCurLoc world ^. objects . at objName

-- Exported (public) functionality used by Repl.hs

-- Navigational actions that don't mutate the world

whereAmI :: World -> Text
whereAmI world = World.describeLoc curLoc
    where curLoc = getCurLoc world

possibleDestinations :: World -> Text
possibleDestinations world = GD.youCanGoTo destinations
    where curLoc = getCurLoc world
          connLocs = curLoc ^. connected
          destinations = articledEnumeration connLocs "the"

lookAround :: World -> Text
lookAround world = case objNames of
                       [] -> GD.nothingToSeeHere
                       names -> GD.youSee $ articledEnumeration names "the"
    where curLoc = getCurLoc world
          objNames = P.map _oname $ M.elems $ curLoc ^. objects

lookAtObject :: World -> [Text] -> Text
lookAtObject world objNameList = case maybeObj of
                                    Nothing -> GD.noObjAround objName
                                    Just obj -> GD.youSeeObject (obj ^. oname) (obj ^. odescription)
    where objName = T.unwords objNameList
          curLoc = getCurLoc world
          maybeObj = curLoc ^. objects . at objName

talkToObject :: World -> [Text] -> Int -> Text
talkToObject world objNameList randInt = case [objExists, objTalks] of
                                            [True, True] -> executeTalk world objName randInt
                                            [True, False] -> GD.objDoesntTalk objName
                                            [False, False] -> GD.noObjToTalkTo objName
                                            [False, True] -> error "World: talkToObject: UNREACHABLE: Object talks but doesn't exist?!"
    where objName = T.unwords objNameList
          maybeObj = getCurLoc world ^. objects . at objName
          objExists = isJust maybeObj
          objTalks = objExists && "talks" `P.elem` (fromJust maybeObj ^. properties)

-- The actions that mutate the world

goTo :: World -> [Text] -> (World, Action)
goTo world place = case [thereAlready, placeExists, placeIsConnected] of
                        [True, _, _] -> (world, Echo $ GD.alreadyThere placeTxt)
                        [_, False, _] -> (world, Echo $ GD.noSuchLoc placeTxt)
                        [_, _, False] -> (world, Echo $ GD.placeNotConnected curLocName placeTxt)
                        _ -> moveBetweenLocs world placeTxt
    where placeTxt = T.unwords place
          curLoc = getCurLoc world
          curLocName = curLoc ^. lname
          thereAlready = T.unwords place == curLocName
          placeExists = isJust $ world ^. locations . at placeTxt
          placeIsConnected = placeTxt `P.elem` (curLoc ^. connected)

-- Initializing the world

initWorld :: World
initWorld = World {
    _player = initPlayer,
    _locations = M.fromList [
        ("forest", initForest),
        ("square", initSquare)
    ]
}