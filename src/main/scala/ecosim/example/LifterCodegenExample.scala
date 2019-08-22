package ecosim.example

import ecosim.deep.codegen.{ClassCreation, InitCreation}
import simulation.core.Actor
import simulation.example.{Farmer, InitClass, Market}
import ecosim.classLifting.Lifter
import ecosim.deep.IR
import ecosim.deep.IR.Predef._
import ecosim.deep.IR.TopLevel._

object LifterCodegenExample extends App {
  val cls1: ClassWithObject[Market] = Market.reflect(IR)
  val cls2: ClassWithObject[Farmer] = Farmer.reflect(IR)
  val cls3: ClassWithObject[InitClass] = InitClass.reflect(IR)

  val startClasses: List[Clasz[_ <: Actor]] = List(cls1, cls2)
  val mainClass = cls3

  val lifter = new Lifter()
  val simulationData = lifter(startClasses, mainClass)

  simulationData._1.foreach({
    case x => {
      val cc = new ClassCreation(x, simulationData._1)
      cc.run()
    }
  })

  val ic = new InitCreation(simulationData._2, simulationData._1)
  ic.run()
}
