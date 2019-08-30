package ecosim.example

import ecosim.classLifting.{Lifter, NBUnit, SpecialInstructions, StatelessServerOptimization}
import ecosim.deep.IR
import ecosim.deep.IR.TopLevel._
import ecosim.deep.codegen.{ClassCreation, InitCreation}
import ecosim.deep.member.Actor
import squid.quasi._

//TODO: make a new annotation for main method?
@lift
class MainClassSSO {
  def main(): List[Actor] = {
    val a: Actor1SSO = new Actor1SSO()
    val b: Actor2SSO = a.actor2
    val c: Actor3SSO = b.actor3
    val d: Actor4SSO = c.actor4
    List(a,b,c,d)
  }
}

//object StatesInitializer {
//  val a: Actor1SSO = StatesInitializer.this.a
//  val b: Actor2SSO = StatesInitializer.this.b
//  val c: Actor3SSO = StatesInitializer.this.c
//  val d: Actor4SSO = StatesInitializer.this.d
//
//}

@lift
class Actor4SSO extends Actor {
  var state = 15
  def getState(): Int = state
  def main() = {
    while(true) {
      SpecialInstructions.handleMessages()
      SpecialInstructions.waitTurns(1)
    }
  }
}

//TODO check what happens if i initialize it here
@lift
class Actor3SSO extends Actor {
  var actor4 = new Actor4SSO()
  def getState(): Int = actor4.getState()
  def main() = {
    while(true) {
      SpecialInstructions.handleMessages()
      SpecialInstructions.waitTurns(1)
    }
  }
}

@lift
class Actor2SSO extends Actor {
  var actor3: Actor3SSO = new Actor3SSO()
  def getState(): Int = actor3.getState()
  def main() = {
    while (true){
      SpecialInstructions.handleMessages()
      SpecialInstructions.waitTurns(1)
    }
  }
}

@lift
class Actor1SSO extends Actor {
  var actor2: Actor2SSO = new Actor2SSO()
  def main() = {
    while(true) {
      println("SENT MESSAGE")
      val a = actor2.getState()
      println("GOT ANSWER", a)
      SpecialInstructions.waitTurns(1)
    }
  }
}


object StatelessServerOptimizationExample extends App {
  val cls1: ClassWithObject[Actor1SSO] = Actor1SSO.reflect(IR)
  val cls2: ClassWithObject[Actor2SSO] = Actor2SSO.reflect(IR)
  val cls3: ClassWithObject[Actor3SSO] = Actor3SSO.reflect(IR)
  val cls4: ClassWithObject[Actor4SSO] = Actor4SSO.reflect(IR)
  val mainClass: ClassWithObject[MainClassSSO] = MainClassSSO.reflect(IR)
  val startClasses: List[Clasz[_ <: Actor]] = List(cls1, cls2, cls3, cls4)
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
