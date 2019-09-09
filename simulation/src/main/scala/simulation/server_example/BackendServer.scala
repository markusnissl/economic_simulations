package simulation.server_example

import ecosim.classLifting.SpecialInstructions
import ecosim.deep.member.Actor
import squid.quasi.lift

@lift
class BackendServer() extends Actor {

  def getContent: String = {
    val r = System.nanoTime().toString
    r
  }

  def main(): Unit = {
    while(true) {
      SpecialInstructions.handleMessages()
      SpecialInstructions.waitTurns(1)
    }
  }
}
