package simulation.core

import simulation.generated.InitData

class Simulation extends App{

  var actors: List[ecosim.runtime.Actor] = List()
  var messages: List[ecosim.runtime.Message] = List()
  var timer = 0
  var until = 10

  def init(): Unit = {
    InitData.initActors
  }

  def main(): Unit = {
    init()

    while (timer < until) {
      val mx = messages.groupBy(_.receiverId)
      actors = actors.map {
        a =>
          a
            .cleanSendMessage
            .addReceiveMessages(mx.getOrElse(a.id, List()))
            .run_until(timer)
      }

      messages = actors.flatMap(_.getSendMessages)
      timer += 1
    }
  }

  main()

}
