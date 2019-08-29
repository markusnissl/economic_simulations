package ecosim.example

import ecosim.classLifting.SpecialInstructions._
import ecosim.deep.member.Actor
import squid.quasi.lift

@lift
class Market extends Actor {
  var goods: List[String] = List[String]()

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

  def main() = {
    while(true) {
      sell(10)
      recursiveTest(List(10, 20, 30))
      handleMessages()
      waitTurns(1)
    }
  }

}
