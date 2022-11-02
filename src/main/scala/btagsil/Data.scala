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
    val connected: Array[String]

class Forest() extends Location:
    val name: String = "forest"
    val description: String = "a beautiful rainforest.\nThere's a fresh smell of nature after rain in the air.\nSunshine is filtering through the tall trees, creating a peaceful feeling inside"
    val connected: Array[String] = Array("square")

class Square() extends Location:
    val name: String = "square"
    val description: String = "a spacious square full of people.\nThe spirit of adventure is in the air!"
    val connected: Array[String] = Array("forest")

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

    def you_went_to(where: String): String = "You went to the " + where + "."

    // Where errors

    def whereWhat(): String = "Where... what?"

    def whereIsWhat(): String = "Where is... what?"

    def dontKnowWhereThingIs(thing: String): String = "No clue where " + thing + " is ¯\\_(ツ)_/¯"

    def wrongWhere(): String = "Nope. Try 'where is <thing>' or 'where am i' instead."

    // Go errors

    def go_to_where(): String = "Go to... where?"

    def can_only_go_to_places(): String = "You can only go 'to' places. Try 'go to <place>'.\nAlso try 'where can i go'."

    def no_such_loc(where: String): String = "You don't see any paths leading to a " + where + " around here."

    def youre_already_there(where: String): String = "You cannot go to the " + where + ", you're already there!"

    def sourceAndDestNotConnected(where: String, from: String): String =
        "You can't go from " + from + " to " + where + ", they're not connected.\nTry 'where can i go' to see where you can go from here."
