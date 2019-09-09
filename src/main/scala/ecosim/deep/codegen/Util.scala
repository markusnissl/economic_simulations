package ecosim.deep.codegen

import ecosim.deep.IR.Predef._
import ecosim.deep.algo.AlgoInfo.{EdgeInfo, VarWrapper}
import ecosim.deep.member.{ActorType, ResponseMessage}
import squid.lib.MutVar

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

case class Pipeline(convertElement: ConvertElement, stateMachineElements: List[StateMachineElement]) {
  def run(): Unit = {
    var stateMachine = convertElement.run()
    stateMachineElements.foreach(x => {
      stateMachine = x.run(stateMachine)
    })
  }
}

abstract class PipelineElement() {}

abstract class ConvertElement(actorTypes: List[ActorType[_]]) extends PipelineElement() {
  def run(): List[CompiledActorGraph]
}

abstract class StateMachineElement() extends PipelineElement() {
  def run(compiledActorGraphs: List[CompiledActorGraph]): List[CompiledActorGraph]
}

case class CompiledActorGraph(var name: String,
                              var graph: ArrayBuffer[EdgeInfo],
                              var variables: List[VarWrapper[_]],
                              var variables2: List[VarValue[_]],
                              var actorTypes: List[ActorType[_]],
                              var positionStack: List[Variable[ListBuffer[List[((Int, Int), Int)]]]], //required to generate poping from stack statements at create code
                              var freePosition: Int = 0,
                              returnValue: List[Variable[MutVar[Any]]],
                              responseMessage: List[Variable[MutVar[ResponseMessage]]],
                             )

/**
  * This class is used to create the variable stack for each parameter variable of a method
  *
  * @param variable to be used in code
  * @param init     code to init variable
  * @param A        type of variable
  * @tparam C variable type
  */
case class VarValue[C](variable: Variable[C], init: OpenCode[C])(implicit val A: CodeType[C])