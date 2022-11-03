package btagsil

// Immutable nagivational and iformational actions

def actWhere(world: World, what: List[String]): String = what match {
    case List() => Text.whereWhat()
    case List("am", "i") => Info currentLocDescription world
    case List("can", "i", "go") => Info possibleDestinations world
    case List("is") => Text.whereIsWhat()
    case "is" :: thing => Text dontKnowWhereThingIs (thing mkString " ")
    case _ => Text.wrongWhere()
}

def actLook(world: World, where: List[String]): String = where match {
    case List() => Text.tryLookingAtSomething()
    case List("around") => Info lookAround world
    case List("at") => Text.lookAtWhat()
    case "at" :: entity => Info.lookAtEntity(world, entity)
    case _ => Text.wrongLook()
}

def actTalk(world: World, target: List[String]): String = target match {
    case List() => Text.talkToSomeonePls()
    case List("to") => Text.talkToWho()
    case "to" :: entity => Info.talkToEntity(world, entity)
    case _ => Text.wrongTalk()
}


// Actions that mutate the world

def actGo(world: World, how: List[String]): String = how match {
    case List() => Text.canOnlyGoToPlaces()
    case List("to") => Text.go_to_where()
    case "to" :: where => Change.go_to_loc(world, where mkString " ")
    case _ => Text.canOnlyGoToPlaces()
}


// REPL's system actions

def replRespond(response: String): Unit =
    println(response + "\n")

def replExit(): Unit =
    println(Text.exitMessage())
    System exit 0

def replEcho(what: List[String]): Unit =
    println(Text echoed (what mkString " "))

def replUnknown(wtf: List[String]): Unit =
    println(Text unknownInput (wtf mkString " "))

def replEmpty(): Unit = {}

def replOnce(world: World, action: List[String]): Unit = action match {
    case List() => replEmpty()
    case List("exit") => replExit()
    case "where" :: what => replRespond(actWhere(world, what))
    case "look" :: where => replRespond(actLook(world, where))
    case "talk" :: target => replRespond(actTalk(world, target))
    case "go" :: how => replRespond(actGo(world, how))
    case "echo" :: what => replEcho(what)

    case wtf => replUnknown(wtf)
}
