package ecosim.example

import ecosim.classLifting.Lifter
import ecosim.deep.IR
import ecosim.deep.IR.TopLevel._
import ecosim.deep.codegen.{CreateActorGraphs, CreateCode, GraphMerge, Pipeline}
import ecosim.deep.member.Actor

object LifterCodegenExample extends App {
  val cls1: ClassWithObject[Market] = Market.reflect(IR)
  val cls2: ClassWithObject[Farmer] = Farmer.reflect(IR)
  val cls3: ClassWithObject[InitClass] = InitClass.reflect(IR)

  val startClasses: List[Clasz[_ <: Actor]] = List(cls1, cls2)
  val mainClass = cls3

  val lifter = new Lifter()
  val simulationData = lifter(startClasses, mainClass)

  val pipeline = Pipeline(new CreateActorGraphs(simulationData._1), List(
    new GraphMerge(),
    new CreateCode(simulationData._2),
  ))

  pipeline.run()
}
