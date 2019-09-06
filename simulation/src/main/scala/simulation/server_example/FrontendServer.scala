package simulation.server_example

import ecosim.classLifting.SpecialInstructions
import ecosim.deep.member.Actor
import squid.quasi.lift

@lift
class FrontendServer() extends Actor {
  var backendServer: BackendServer = null

  def requestPage(): String = {
    val serverTime = backendServer.getContent
    "<html>"+serverTime+"</html>"
  }

  def main(): Unit = {
    while(true) {
      requestPage()
      //SpecialInstructions.handleMessages()
      SpecialInstructions.waitTurns()
    }
  }

}
