package btagsil

import scala.io.StdIn.readLine

def mainLoop(): Unit = {
    print("ScB >> ")
    val input = readLine().split(" ").filter(s => s.nonEmpty).toList

    replOnce(input)

    mainLoop()
}

object Game extends App {
    mainLoop()
}