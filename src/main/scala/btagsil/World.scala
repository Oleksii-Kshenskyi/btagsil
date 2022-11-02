package btagsil

import scala.collection.mutable.Map

private def articledEnumeration(what: Array[String], article: String) =
    what.map(s => article + " " + s) mkString ", "

private def getCurrentLoc(world: World): Location =
    world.locations(world.player.currentLocation)

private def move_to_loc(world: World, where: String): String =
    world.player.currentLocation = where
    Text you_went_to where

object Change:
    def go_to_loc(world: World, where: String): String =
        val currentLoc = getCurrentLoc(world)
        val destinationExists = world.locations.contains(where)
        val alreadyThere = where == currentLoc.name
        val destinationIsConnected = currentLoc.connected.contains(where)

        Array(destinationExists, alreadyThere, destinationIsConnected) match {
            case Array(false, _, _) => Text no_such_loc where
            case Array(_, true, _) => Text youre_already_there where
            case Array(_, _, false) => Text.sourceAndDestNotConnected(where, currentLoc.name)
            case _ => move_to_loc(world, where)
        }

object Info:
    def currentLocDescription(world: World): String =
        val currentLoc = getCurrentLoc(world)
        Text.locDescription(currentLoc.name, currentLoc.description)

    def possibleDestinations(world: World): String =
        val connectedText = articledEnumeration(getCurrentLoc(world).connected, "the")
        Text you_can_go_here connectedText

def initWorld(): World =
    World(Player(),
          Map("forest" -> Forest(),
              "square" -> Square()))
