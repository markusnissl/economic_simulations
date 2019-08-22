package simulation.example

import simulation.core.Actor
import squid.quasi.lift

@lift
class Market extends Actor {
  var goods: List[String] = Nil

  def sell(unit: Int): Unit = {
    println("Market sells: " + unit)
  }

  def sell2(unit: Int): Int = {
    println("Market sells: " + unit)
    42
  }

  def recursiveTest(l: List[Int]): Unit = {
    if (l.isEmpty) {
    } else {
      recursiveTest(l.tail)
      println(l.head)
    }
  }

}
