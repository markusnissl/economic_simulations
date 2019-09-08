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
    //var x = 0
    while(true) {
      handleMessages()
      //x = x + 1
      //println("Binding test:", x)
      sell(10)
      recursiveTest(List(10, 20, 30))
      waitTurns(1)
    }
  }

}
