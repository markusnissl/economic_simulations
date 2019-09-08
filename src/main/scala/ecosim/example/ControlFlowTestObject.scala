package ecosim.example

import ecosim.classLifting.SpecialInstructions.waitTurns
import ecosim.deep.member.Actor
import squid.quasi.lift

@lift
class ControlFlowTestObject extends Actor {

  var x = 0
  var y = 0

  def main(): Unit = {
    while(true) {
      if (x < 0) {
        waitTurns(1)
      } else {
        if (x < 0) {
          waitTurns(1)
        }
      }
      waitTurns(1)
    }
  }

}
