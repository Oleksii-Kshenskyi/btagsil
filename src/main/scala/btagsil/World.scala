package btagsil

import scala.collection.mutable.Map
import scala.util.Random

private def articledEnumeration(what: Array[String], article: String) =
    what.map(s => article + " " + s) mkString ", "

private def getCurrentLoc(world: World): Location =
    world.locations(world.player.currentLocation)

private def move_to_loc(world: World, where: String): String =
    world.player.currentLocation = where
    Text you_went_to where

private def take_from_pandoras_box(theBox: Array[String]): String =
    val index = Random.nextInt(theBox.length)
    theBox(index)

private def talkToGuard(world: World, theEntity: Entity): String =
    val theBox: Array[String] = theEntity.behavior("pandora's box").asInstanceOf[Array[String]]
    Text.entity_says(theEntity.name.capitalize, take_from_pandoras_box(theBox))

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

    def lookAround(world: World): String =
        val objects: Array[String] = getCurrentLoc(world).objects.keys.toArray
        if objects.nonEmpty then Text.you_see_those(articledEnumeration(objects, "the")) else Text.you_see_nothing()

    def lookAtEntity(world: World, entityList: List[String]): String =
        val entityName = entityList mkString " "
        val currentLoc = getCurrentLoc(world)

        val entityExists = currentLoc.objects.contains(entityName)
        val entity = if entityExists then Some(currentLoc.objects(entityName)) else None
        if entityExists then Text.you_see_object(entity.get.name, entity.get.description) else Text.no_object_to_look_at(entityName)

    def objectHasProp(entity: Entity, prop: String): Boolean =
        entity.properties.contains(prop)

    def talkToEntity(world: World, entity: List[String]): String =
        val entityName = entity mkString " "
        val currentObjects = getCurrentLoc(world).objects
        val entityExists = currentObjects.contains(entityName)
        val theEntity = if entityExists then Some(currentObjects(entityName)) else None
        val entityTalks = if entityExists then objectHasProp(theEntity.get, "talks") else false
        entity match {
            case List() => throw Exception("Info.talkToEntity(): UNREACHABLE: at this point the entity name should be set!")
            case List("guard") => talkToGuard(world, theEntity.get)
            case _ => Text.no_such_entity_to_talk_to(entity mkString " ")
        }


def initWorld(): World =
    World(Player(),
          Map("forest" -> Forest(),
              "square" -> Square()))
