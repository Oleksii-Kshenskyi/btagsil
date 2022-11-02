package btagsil

// Immutable nagivational and iformational actions

def actWhere(world: World, what: List[String]): String = what match {
    case List() => Text.whereWhat()
    case List("am", "i") => Info currentLocDescription world
    case List("is") => Text.whereIsWhat()
    case "is" :: thing => Text dontKnowWhereThingIs (thing mkString " ")
    case _ => Text.wrongWhere()
}

// Actions that mutate the world

def actGo(world: World, how: List[String]): String = how match {
    case List() => Text.can_only_go_to_places()
    case List("to") => Text.go_to_where()
    case "to" :: where => Change.go_to_loc(world, where mkString " ")
    case _ => Text.can_only_go_to_places()
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
    case "go" :: how => replRespond(actGo(world, how))
    case "echo" :: what => replEcho(what)
    case wtf => replUnknown(wtf)
}
