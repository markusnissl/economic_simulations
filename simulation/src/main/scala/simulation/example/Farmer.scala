package simulation.example

import simulation.core.Actor
import squid.quasi.lift
import ecosim.classLifting.SpecialInstructions._

@lift
class Farmer() extends Actor {
  var happiness: Int = new Integer(1)
  var peers: List[Farmer] = List[Farmer]()
  var market: Market = new Market()

  def tell(actor: Actor, h: Int): Unit = {
    happiness = happiness - h
  }

  def notifyPeers(): Unit = {
    peers.foreach { p =>
      p.tell(this, happiness)
    }
  }

  def main() = {
    while(true) {
      val testResult = market.sell2(500)
      println("TEST_VAR", testResult)
      waitTurns(1)
    }
  }

}