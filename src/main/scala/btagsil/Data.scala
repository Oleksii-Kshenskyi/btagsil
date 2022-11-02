package btagsil

import scala.collection.mutable.Map

trait Weapon:
    val name: String
    val description: String

class Fists() extends Weapon:
    val name: String = "fists"
    val description: String = "just your bare fists"

class Player():
    var currentLocation: String = "forest"
    var weapon: Weapon = Fists()

trait Location:
    val name: String
    val description: String

class Forest() extends Location:
    val name: String = "forest"
    val description: String = "a beautiful rainforest.\nThere's a fresh smell of nature after rain in the air.\nSunshine is filtering through the tall trees, creating a peaceful feeling inside"

class World(p: Player, ls: Map[String, Location]):
    var player: Player = p
    var locations: Map[String, Location] = ls

object Text:
    def exitMessage(): String = "Thanks for playing! See you soon!\n"

    def echoed(what: String): String = "Echoed => '" + what + "';\n"

    def unknownInput(wtf: String): String = "Wait... What is " + wtf + "?\n"

    // Location helpers

    def locDescription(name: String, description: String): String =
        "You're in the " + name + ". It's " + description + "."

    // Where errors

    def whereWhat(): String = "Where... what?"

    def whereIsWhat(): String = "Where is... what?"

    def dontKnowWhereThingIs(thing: String): String = "No clue where " + thing + " is ¯\\_(ツ)_/¯"

    def wrongWhere(): String = "Nope. Try 'where is <thing>' or 'where am i' instead."
