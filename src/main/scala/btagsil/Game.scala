package btagsil

import scala.io.StdIn.readLine

def mainLoop(): Unit = {
    var world = initWorld()

    print("ScB >> ")
    val input = readLine().split(" ").filter(s => s.nonEmpty).toList

    replOnce(world, input)

    mainLoop()
}

object Game extends App {
    mainLoop()
}