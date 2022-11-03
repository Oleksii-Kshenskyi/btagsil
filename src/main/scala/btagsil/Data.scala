package btagsil

import scala.collection.mutable.Map

def guardsPandorasBox(): Array[String] = Array(
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
    "I remember that time I was a wee little lad...",
)

trait Weapon:
    val name: String
    val description: String

class Fists() extends Weapon:
    val name: String = "fists"
    val description: String = "just your bare fists"

class Player():
    var currentLocation: String = "forest"
    var weapon: Weapon = Fists()

trait Entity:
    val name: String
    val description: String
    val properties: List[String]
    val behavior: Map[String, Any]

class Guard() extends Entity:
    val name: String = "guard"
    val description: String = "a big burly man in heavy armor wielding a halberd.\nHe seems friendly and willing to chat."
    val properties: List[String] = List("talks")
    val behavior: Map[String, Any] = Map("pandora's box" -> guardsPandorasBox())

trait Location:
    val name: String
    val description: String
    val connected: Array[String]
    var objects: Map[String, Entity]

class Forest() extends Location:
    val name: String = "forest"
    val description: String = "a beautiful rainforest.\nThere's a fresh smell of nature after rain in the air.\nSunshine is filtering through the tall trees, creating a peaceful feeling inside"
    val connected: Array[String] = Array("square")
    var objects: Map[String, Entity] = Map()

class Square() extends Location:
    val name: String = "square"
    val description: String = "a spacious square full of people.\nThe spirit of adventure is in the air!"
    val connected: Array[String] = Array("forest")
    var objects: Map[String, Entity] = Map("guard" -> Guard())

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

    def you_can_go_here(places: String): String = "You can go to " + places + " from here."

    // Object helpers

    def you_see_those(things: String): String = "You see " + things + " here."

    def you_see_object(name: String, description: String): String =
        "You see a " + name + ". It's " + description + "."

    def entity_says(name: String, saysWhat: String): String =
        name + " says: '" + saysWhat + "'"

    // Where errors

    def whereWhat(): String = "Where... what?"

    def whereIsWhat(): String = "Where is... what?"

    def dontKnowWhereThingIs(thing: String): String = "No clue where " + thing + " is ¯\\_(ツ)_/¯"

    def wrongWhere(): String = "Nope. Try 'where is <thing>' or 'where am i' instead."

    // Look errors

    def wrongLook(): String = "How does one look that way?\nTry 'look around' or 'look at <object>'."

    def tryLookingAtSomething(): String = "Look... where?\nTry 'look around' or 'look at <object>'."

    def lookAtWhat(): String = "Look at... what?"

    def no_object_to_look_at(entityName: String): String = "You don't see any " + entityName + "s around."

    def you_see_nothing(): String = "You don't see anything of importance here."

    // Talk errors

    def talkToSomeonePls(): String = "Okay, I'm talking now. Be more specific though.\nTry 'talk to <someone>'."

    def talkToWho(): String = "Talk to... Who?"

    def wrongTalk(): String = "Try talking 'to' someone. Try looking around to see who's around."

    def no_such_entity_to_talk_to(entityStr: String): String =
        "You don't see any " + entityStr + "s around that would be willing to chat."

    // Go errors

    def go_to_where(): String = "Go to... where?"

    def can_only_go_to_places(): String = "You can only go 'to' places. Try 'go to <place>'.\nAlso try 'where can i go'."

    def no_such_loc(where: String): String = "You don't see any paths leading to a " + where + " around here."

    def youre_already_there(where: String): String = "You cannot go to the " + where + ", you're already there!"

    def sourceAndDestNotConnected(where: String, from: String): String =
        "You can't go from " + from + " to " + where + ", they're not connected.\nTry 'where can i go' to see where you can go from here."
