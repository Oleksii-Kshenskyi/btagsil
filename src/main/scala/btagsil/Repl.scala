package btagsil

// REPL's system actions

def replExit(): Unit = {
    println(Text.exitMessage())
    System exit 0
}

def replEcho(what: List[String]): Unit =
    println(Text echoed (what mkString " "))

def replUnknown(wtf: List[String]): Unit =
    println(Text unknownInput (wtf mkString " "))

def replEmpty(): Unit = {}

def replOnce(action: List[String]): Unit = action match {
    case List() => replEmpty()
    case List("exit") => replExit()
    case "echo" :: what => replEcho(what)
    case wtf => replUnknown(wtf)
}
