package simulation.server_example

import ecosim.deep.member.Actor
import squid.quasi.lift

import scala.collection.mutable.ListBuffer

@lift
class MainInit {
  def main(): List[Actor] = {
    val l = ListBuffer[Actor]()

    for(i <- 0 to 100000) {
      val backendServer = new BackendServer
      val frontendServer = new FrontendServer
      frontendServer.backendServer = backendServer
      //val client = new Client
      //client.frontendServer = frontendServer

      l.append(backendServer)
      l.append(frontendServer)
      //l.append(client)
    }

    l.toList

  }
}
