package btagsil

import scala.io.StdIn.readLine

def mainLoop(): Unit = {
    print("SCB>> ")
    val input = readLine().split(" ").filter(s => s.nonEmpty).toList

    replOnce(input)

    mainLoop()
}

object Game extends App {
    mainLoop()
}