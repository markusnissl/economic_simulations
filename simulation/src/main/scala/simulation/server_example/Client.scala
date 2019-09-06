package simulation.server_example

import ecosim.classLifting.SpecialInstructions
import ecosim.deep.member.Actor
import squid.quasi.lift

@lift
class Client() extends Actor {
  var frontendServer:FrontendServer = null

  def main(): Unit = {
    while(true) {
      frontendServer.requestPage()
      SpecialInstructions.waitTurns()
    }
  }
}
