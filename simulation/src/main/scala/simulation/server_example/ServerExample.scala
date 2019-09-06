package simulation.server_example

import ecosim.classLifting.Lifter
import ecosim.deep.IR
import ecosim.deep.codegen.{CreateActorGraphs, CreateCode, GraphMerge, Pipeline}
import ecosim.deep.member.Actor
import ecosim.deep.IR.TopLevel._

object ServerExample extends App {
  val cls1: ClassWithObject[BackendServer] = BackendServer.reflect(IR)
  val cls2: ClassWithObject[FrontendServer] = FrontendServer.reflect(IR)
  val cls3: ClassWithObject[Client] = Client.reflect(IR)
  val mainClass: ClassWithObject[MainInit] = MainInit.reflect(IR)
  val startClasses: List[Clasz[_ <: Actor]] = List(cls1, cls2, cls3)
  val lifter = new Lifter()
  val simulationData = lifter(startClasses, mainClass)

  val pipeline = Pipeline(new CreateActorGraphs(simulationData._1), List(
    new GraphMerge(),
    new CreateCode(simulationData._2),
  ))

  pipeline.run()
}
