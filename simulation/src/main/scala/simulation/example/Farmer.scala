package simulation.example

import ecosim.deep.member.Actor
import squid.quasi.lift

@lift
class Farmer() extends Actor {
  var happiness = 0
  var peers: List[Farmer] = Nil
  var market: Market = null

  def tell(actor: Actor, h: Int): Unit = {
    happiness = happiness - h
  }

  def notifyPeers(): Unit = {
    peers.foreach { p =>
      p.tell(this, happiness)
    }
  }

}