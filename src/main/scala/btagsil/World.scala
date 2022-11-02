package btagsil

import scala.collection.mutable.Map

private def getCurrentLoc(world: World): Location =
    world.locations(world.player.currentLocation)

object Info:
    def currentLocDescription(world: World): String =
        val currentLoc = getCurrentLoc(world)
        Text.locDescription(currentLoc.name, currentLoc.description)

def initWorld(): World =
    World(Player(),
          Map("forest" -> Forest()))
