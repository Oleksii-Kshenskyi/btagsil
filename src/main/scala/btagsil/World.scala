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

private def weSellThis(world: World, sellerEntity: Entity): String =
    val goodiesNames = sellerEntity.behavior("sells").asInstanceOf[Map[String, Weapon]].keys.toArray
    Text weSellThese articledEnumeration(goodiesNames, "the")

private def performPurchase(world: World, thing: String, sellerName: String): String =
    val sellerObject = getCurrentLoc(world).objects(sellerName)
    val thingSold = sellerObject.behavior("sells").asInstanceOf[Map[String, Weapon]](thing)
    world.player.weapon = thingSold
    Text.boughtThingFromSeller(sellerObject.name.capitalize, thing)

private def objectHasProp(world: World, entity: String, prop: String): Boolean =
        getCurrentLoc(world).objects(entity).properties.contains(prop)

private def performMutualSmacking(player: Player, monster: Entity, playerNewHP: Integer, monsterNewHP: Integer): String =
    player.hp = playerNewHP
    monster.behavior("hp") = monsterNewHP
    Text.mutuallySmacked(monster.name, monster.behavior("swing").asInstanceOf[String], player.weapon.swing, playerNewHP, monsterNewHP)

private def youWin(player: Player, monster: Entity, playerNewHP: Integer, monsterNewHP: Integer): String =
    performMutualSmacking(player, monster, playerNewHP, monsterNewHP) +
    Text.youWon(monster.name)

private def youAreDed(monster: Entity): Unit =
    println(Text.youAreDed(monster.name, monster.behavior("swing").asInstanceOf[String]) + ".\n\n")
    System.exit(0)

private def checkWhoDiesAndSmack(world: World, entityName: String): String =
    var monster = getCurrentLoc(world).objects(entityName)
    val monsterHP = monster.behavior("hp").asInstanceOf[Integer]
    val playerNewHP = world.player.hp - monster.behavior("damage").asInstanceOf[Integer]
    val monsterNewHP = monsterHP - world.player.weapon.damage

    val alreadyDead = monsterHP <= 0
    val playerDies = playerNewHP <= 0
    val monsterDies = monsterNewHP <= 0

    Array(alreadyDead, playerDies, monsterDies) match {
        case Array(true, _, _) => Text.stopItsAlreadyDead(monster.name)
        case Array(false, false, false) => performMutualSmacking(world.player, monster, playerNewHP, monsterNewHP)
        case Array(false, false, true) => youWin(world.player, monster, playerNewHP, monsterNewHP)
        case Array(false, true, _) => youAreDed(monster).toString()
        case _ => throw Exception("checkWhoDiesAndSmack(): UNREACHABLE: the state of player/monster dying is unclear, debug further from here.")
    }

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

    def buy(world: World, thing: String, seller: List[String]): String =
        val sellerName = seller mkString " "
        val currentObjects = getCurrentLoc(world).objects

        val sellerExists = currentObjects.contains(sellerName)
        val sellerObject = if sellerExists then Some(currentObjects(sellerName)) else None
        val sellerSells = if sellerExists then currentObjects(sellerName).properties.contains("sells") else false
        val sellerSellsThing = if sellerSells then sellerObject.get.behavior("sells").asInstanceOf[Map[String, Weapon]].contains(thing) else false
        Array(sellerExists, sellerSells, sellerSellsThing) match {
            case Array(false, _, _) => Text.sellerDoesNotExist(sellerName)
            case Array(_, false, _) => Text.sellerDoesntSell(sellerName)
            case Array(_, _, false) => Text.sellerDoesntSellThing(sellerName, thing)
            case _ => performPurchase(world, thing, sellerName)
        }

    def smack(world: World, target: List[String]): String =
        val entityName = target mkString " "
        val currentLoc = getCurrentLoc(world)
        val targetExistsOnLoc = currentLoc.objects.contains(entityName)
        val targetFights = if targetExistsOnLoc then objectHasProp(world, entityName, "fights") else false

        Array(targetExistsOnLoc, targetFights) match {
            case Array(false, false) => Text.noSuchTargetToSmack(entityName)
            case Array(true, false) => Text.badIdeaToSmack(entityName)
            case Array(true, true) => checkWhoDiesAndSmack(world, entityName)
            case Array(false, true) => throw Exception("Change.smack(): UNREACHABLE: Target doesn't exist but can fight?!")
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

    def chooseEntityAndTalk(world: World, entityName: String): String =
        val theEntity = getCurrentLoc(world).objects(entityName)
        entityName match {
            case "guard" => talkToGuard(world, theEntity)
            case "shopkeeper" => Text.entitySays(theEntity.name.capitalize, Text.shopkeeperLine())
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

    def purchaseOptions(world: World): String =
        val sellers = getCurrentLoc(world).objects.values.filter(o => objectHasProp(world, o.name, "sells")).toList
        sellers.length match {
            case 0 => Text.noSellersAround()
            case 1 => weSellThis(world, sellers(0))
            case _ => throw Exception("Info.purchaseOptions(): UNREACHABLE: more than one seller on location?!")
        }


def initWorld(): World =
    World(Player(),
          Map("forest" -> Forest(),
              "square" -> Square(),
              "weapon shop" -> WeaponShop(),
              "cave" -> Cave()))
