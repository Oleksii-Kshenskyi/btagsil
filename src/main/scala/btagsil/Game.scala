package btagsil

import scala.io.StdIn.readLine

def mainLoop(): Unit = {
    print("SCB>> ")
    val input = readLine().split(" ").filter(s => s.nonEmpty)
    
    input match {
        case Array("exit") => println("Thanks for playing!\n"); System.exit(0)
        case _ => println("Echoed >> '" + input.mkString(" ") + "';")
    }
    mainLoop()
}

object Game extends App {
    mainLoop()
}