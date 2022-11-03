package btagsil

import scala.collection.mutable.Map
import scala.util.Random

private def articledEnumeration(what: Array[String], article: String) =
    what.map(s => article + " " + s) mkString ", "

private def getCurrentLoc(world: World): Location =
    world.locations(world.player.currentLocation)

private def move_to_loc(world: World, where: String): String =
    world.player.currentLocation = where
    Text youWentTo where

private def take_from_pandoras_box(theBox: Array[String]): String =
    val index = Random.nextInt(theBox.length)
    theBox(index)

private def talkToGuard(world: World, theEntity: Entity): String =
    val theBox: Array[String] = theEntity.behavior("pandora's box").asInstanceOf[Array[String]]
    Text.entitySays(theEntity.name.capitalize, take_from_pandoras_box(theBox))

object Change:
    def go_to_loc(world: World, where: String): String =
        val currentLoc = getCurrentLoc(world)
        val destinationExists = world.locations.contains(where)
        val alreadyThere = where == currentLoc.name
        val destinationIsConnected = currentLoc.connected.contains(where)

        Array(destinationExists, alreadyThere, destinationIsConnected) match {
            case Array(false, _, _) => Text noSuchLoc where
            case Array(_, true, _) => Text youreAlreadyThere where
            case Array(_, _, false) => Text.sourceAndDestNotConnected(where, currentLoc.name)
            case _ => move_to_loc(world, where)
        }

object Info:
    def currentLocDescription(world: World): String =
        val currentLoc = getCurrentLoc(world)
        Text.locDescription(currentLoc.name, currentLoc.description)

    def possibleDestinations(world: World): String =
        val connectedText = articledEnumeration(getCurrentLoc(world).connected, "the")
        Text youCanGoHere connectedText

    def lookAround(world: World): String =
        val objects: Array[String] = getCurrentLoc(world).objects.keys.toArray
        if objects.nonEmpty then Text.youSeeThose(articledEnumeration(objects, "the")) else Text.youSeeNothing()

    def lookAtEntity(world: World, entityList: List[String]): String =
        val entityName = entityList mkString " "
        val currentLoc = getCurrentLoc(world)

        val entityExists = currentLoc.objects.contains(entityName)
        val entity = if entityExists then Some(currentLoc.objects(entityName)) else None
        if entityExists then Text.youSeeEntity(entity.get.name, entity.get.description) else Text.noObjectToLookAt(entityName)

    def objectHasProp(world: World, entity: String, prop: String): Boolean =
        getCurrentLoc(world).objects(entity).properties.contains(prop)

    def chooseEntityAndTalk(world: World, entityName: String): String = entityName match {
        case "guard" => talkToGuard(world, getCurrentLoc(world).objects(entityName))
        case _ => throw Exception("Info.chooseEntityAndTalk(): there should have been a valid talker entity to talk to at this point, but there isn't.")
    }

    def talkToEntity(world: World, entity: List[String]): String =
        val entityName = entity mkString " "
        val currentObjects = getCurrentLoc(world).objects
        val entityExists = currentObjects.contains(entityName)
        val entityTalks = if entityExists then objectHasProp(world, entityName, "talks") else false
        Array(entityExists, entityTalks) match {
            case Array(true, true) => chooseEntityAndTalk(world, entityName)
            case Array(true, false) => Text.isNotTalkable(entityName)
            case Array(false, false) => Text noSuchEntityToTalkTo (entity mkString " ")
            case _ => throw Exception("Info.talkToEntity(): UNREACHABLE: entity talks but doesn't exist?")
        }


def initWorld(): World =
    World(Player(),
          Map("forest" -> Forest(),
              "square" -> Square()))
