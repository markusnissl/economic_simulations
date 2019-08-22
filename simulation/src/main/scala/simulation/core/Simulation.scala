package simulation.core

import simulation.generated.InitData

object Simulation extends App {

  var actors: List[Actor] = List()
  var messages: List[Message] = List()
  var timer = 0
  var until = 10

  def init(): Unit = {
    actors = InitData.initActors
  }

  def main(): Unit = {
    init()

    while (timer <= until) {
      println("TIMER", timer)
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
