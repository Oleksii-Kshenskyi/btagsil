package btagsil

import scala.io.StdIn.readLine

def mainLoop(world: World): Unit = {
    print("ScB >> ")
    val input = readLine().split(" ").filter(s => s.nonEmpty).toList

    replOnce(world, input)

    mainLoop(world)
}

object Game extends App {
    mainLoop(initWorld())
}