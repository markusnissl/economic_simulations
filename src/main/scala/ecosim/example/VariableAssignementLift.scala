package ecosim.example

import ecosim.deep.member.Actor
import squid.quasi.lift
import ecosim.deep.IR
import IR.TopLevel._
import ecosim.classLifting.Lifter
import ecosim.classLifting.SpecialInstructions._

@lift
class Clas1_stateless extends Actor {
  def main() = {
    var ERMENTRAUT = "string"
    println(ERMENTRAUT)
    waitTurns(100)
  }
}

@lift
class InitC {
  def init() = {
    List(new Clas1_stateless())
  }
}

object VariableAssignementLift extends App {
  val cls1: ClassWithObject[Clas1_stateless] = Clas1_stateless.reflect(IR)
  val cls2: ClassWithObject[InitC] = InitC.reflect(IR)
  val lifter = new Lifter()
  val liftedData = lifter(List(cls1), cls2)
  println(liftedData)
}
