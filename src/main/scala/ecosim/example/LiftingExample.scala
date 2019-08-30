package ecosim.example

import ecosim.classLifting.{Lifter, NBUnit, SpecialInstructions, StatelessServerOptimization}
import ecosim.deep.IR
import ecosim.deep.IR.TopLevel._
import ecosim.deep.codegen.{ClassCreation, InitCreation}
import ecosim.deep.member.Actor
import squid.quasi._

//TODO: make a new annotation for main method?
@lift
class MainClass {
  def main(): List[Actor] = {
    val b = new Actor2()
    val a = new Actor1()
    a.actor2 = b
    List(b,a)
  }
}

@lift
class Actor2 extends Actor {
  var a = 5
  def met2(par1: String) = {
    println(par1)
  }
  def sell(number: Int, price: Int)(msg: String): Int = {
    println(msg)
    a
  }
  def main() = {
    while (true){
//      sell(1,22)("i sell stuff")
      SpecialInstructions.waitTurns(1)
      SpecialInstructions.handleMessages()
    }
  }
}

@lift
class Actor1() extends Actor {
  var c = new Integer(10)
  var g = new String("string")
  var actor2: Actor2 = new Actor2()
  def main() = {
    while(true) {
      val a = actor2.sell(1,22)("i buy stuff")
      println(a)
      SpecialInstructions.waitTurns(1)
    }
  }
  def met3(): String = {actor2.met2("afk"); "afk"}
  def met1(par1: Int, par2: Int)(par3: Int) = {
    println("whoa")
    3
  }
}


object LiftingExample extends App {
  val cls1: ClassWithObject[Actor1] = Actor1.reflect(IR)
  val cls2: ClassWithObject[Actor2] = Actor2.reflect(IR)
  val cls3: ClassWithObject[MainClass] = MainClass.reflect(IR)
  val startClasses: List[Clasz[_ <: Actor]] = List(cls1, cls2)
  val mainClass = cls3
  val lifter = new Lifter()
  val a = lifter(startClasses, mainClass)
  val simulationData = (StatelessServerOptimization.optimizeActors(a._1), a._2)

  simulationData._1.foreach({
    case x => {
      val cc = new ClassCreation(x, simulationData._1)
      cc.run()
    }
  })

  val ic = new InitCreation(simulationData._2, simulationData._1)
  ic.run()
}
