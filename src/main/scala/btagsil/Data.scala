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

class Axe() extends Weapon:
    val name: String = "axe"
    val description: String = "a humongous razor-sharp double-headed greataxe"

class Sword() extends Weapon:
    val name: String = "sword"
    val description: String = "a gorgeous long ornamented claymore"

class Bow() extends Weapon:
    val name: String = "bow"
    val description: String = "a gigantic greatbow that uses spears as arrows"

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

class Shopkeeper() extends Entity:
    val name: String = "shopkeeper"
    val description: String = "an old man with an eye towards trade.\nHe's watching you closely hoping you'll buy something from him."
    val properties: List[String] = List("talks", "sells")
    val behavior: Map[String, Any] = Map("sells" -> Map("axe" -> Axe(), "sword" -> Sword(), "bow" -> Bow()))

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
    val connected: Array[String] = Array("forest", "weapon shop")
    var objects: Map[String, Entity] = Map("guard" -> Guard())

class WeaponShop() extends Location:
    val name: String = "weapon shop"
    val description: String = "a spacious stone building filled to the brim with weapons to buy.\nThe shopkeeper is watching you closely.\nAn axe, a sword and a bow catch your eye."
    val connected: Array[String] = Array("square")
    var objects: Map[String, Entity] = Map("shopkeeper" -> Shopkeeper())

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

    def youWentTo(where: String): String = "You went to the " + where + "."

    def youCanGoHere(places: String): String = "You can go to " + places + " from here."

    // Object helpers

    def youSeeThose(things: String): String = "You see " + things + " here."

    def youSeeEntity(name: String, description: String): String =
        "You see a " + name + ". It's " + description + "."

    def youSeeYourWeapon(description: String): String = "You see " + description + "."

    def entitySays(name: String, saysWhat: String): String =
        name + " says: '" + saysWhat + "'"

    def weSellThese(things: String): String = "You can buy " + things + " here."

    def shopkeeperLine(): String = "Stop talking and buy something already, you flirtatious vagabond!"

    def boughtThingFromSeller(sellerName: String, thing: String): String = sellerName + " says: 'Thanks for buying the " + thing + "!'"

    // Where errors

    def whereWhat(): String = "Where... what?"

    def whereIsWhat(): String = "Where is... what?"

    def dontKnowWhereThingIs(thing: String): String = "No clue where " + thing + " is ¯\\_(ツ)_/¯"

    def wrongWhere(): String = "Nope. Try 'where is <thing>' or 'where am i' instead."

    // What errors

    def whatWhat(): String = "What 'what'?"

    def whatIsWhat(): String = "What is what?"

    def wrongWhat(): String = "What's that? Try 'what is <something>' or 'what can i buy'."

    def dontKnowWhatThingIs(thing: String): String = "No clue what " + thing + " is ¯\\_(ツ)_/¯"

    def noSellersAround(): String = "You don't see anyone willing to sell you anything around here."

    // Look errors

    def wrongLook(): String = "How does one look that way?\nTry 'look around' or 'look at <object>'."

    def tryLookingAtSomething(): String = "Look... where?\nTry 'look around' or 'look at <object>'."

    def lookAtWhat(): String = "Look at... what?"

    def noObjectToLookAt(entityName: String): String = "You don't see any " + entityName + "s around."

    def youSeeNothing(): String = "You don't see anything of importance here."

    // Talk errors

    def talkToSomeonePls(): String = "Okay, I'm talking now. Be more specific though.\nTry 'talk to <someone>'."

    def talkToWho(): String = "Talk to... Who?"

    def wrongTalk(): String = "Try talking 'to' someone. Try looking around to see who's around."

    def noSuchEntityToTalkTo(entityStr: String): String =
        "You don't see any " + entityStr + "s around that would be willing to chat."
    
    def isNotTalkable(entityName: String): String = "The idea of talking to you doesn't excite the " + entityName + " that much."

    // Go errors

    def go_to_where(): String = "Go to... where?"

    def canOnlyGoToPlaces(): String = "You can only go 'to' places. Try 'go to <place>'.\nAlso try 'where can i go'."

    def noSuchLoc(where: String): String = "You don't see any paths leading to a " + where + " around here."

    def youreAlreadyThere(where: String): String = "You cannot go to the " + where + ", you're already there!"

    def sourceAndDestNotConnected(where: String, from: String): String =
        "You can't go from " + from + " to " + where + ", they're not connected.\nTry 'where can i go' to see where you can go from here."

    // Buy errors

    def buyWhat(): String = "Buy... what?"

    def needToBuyFromSomeone(): String = "You can only buy something 'from' someone.\nTry 'buy <thing> from <someone>'."

    def buyFromWho(thing: String): String = "Buy " + thing + " from who?"

    def sellerDoesNotExist(sellerName: String): String = "You don't see any " + sellerName + "s around to buy stuff from."

    def sellerDoesntSell(sellerName: String): String = "The " + sellerName + " doesn't seem interested in selling you stuff."

    def sellerDoesntSellThing(sellerName: String, thing: String): String = "The " + sellerName + " doesn't have any " + thing + "s to sell you."
