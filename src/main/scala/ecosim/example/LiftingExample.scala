package ecosim.example

import ecosim.classLifting.{Lifter, NBUnit, SpecialInstructions}
import ecosim.deep.IR
import ecosim.deep.IR.TopLevel._
import ecosim.deep.codegen.{CreateActorGraphs, CreateCode, GraphMerge, Pipeline, SSO}
import ecosim.deep.member.Actor
import squid.quasi._

//TODO: make a new annotation for main method?
@lift
class MainClass {
  def main(): List[Actor] = {
    val b = new Actor2stateless()
    val a = new Actor1()
    a.actor2 = b
    List(b, a)
  }
}

@lift
class Actor2stateless extends Actor {
  def met2(par1: String) = {
    println(par1)
  }

  def main() = {
    while (true) {
      sell(1,22)("i sell stuff")
      SpecialInstructions.waitTurns(1)
      SpecialInstructions.handleMessages()
    }
  }

  def sell(number: Int, price: Int)(msg: String): NBUnit = {
    println(msg); NBUnit()
  }
}

@lift
class Actor1() extends Actor {
  val a = List[Int](1, 2, 3)
  val b = List[Int](2, 3, 4)
  var c = new Integer(10)
  var g = new String("string")
  var actor2: Actor2stateless = new Actor2stateless()

  def main() = {
    while (true) {
      actor2.sell(1,22)("i buy stuff")
      actor2.sell(1,22)("i buy sssss")
      SpecialInstructions.waitTurns(1)
    }
  }

  def met3(): String = {
    actor2.met2("afk"); "afk"
  }

  def met1(par1: Int, par2: Int)(par3: Int) = {
    println("whoa")
    3
  }
}


object LiftingExample extends App {
  val cls1: ClassWithObject[Actor1] = Actor1.reflect(IR)
  val cls2: ClassWithObject[Actor2stateless] = Actor2stateless.reflect(IR)
  val cls3: ClassWithObject[MainClass] = MainClass.reflect(IR)
  val startClasses: List[Clasz[_ <: Actor]] = List(cls1, cls2)
  val mainClass = cls3
  val lifter = new Lifter()
  val simulationData = lifter(startClasses, mainClass)

  val pipeline = Pipeline(new CreateActorGraphs(simulationData._1), List(
    new SSO(),
    new GraphMerge(),
    new CreateCode(simulationData._2),
  ))

  pipeline.run()
}
