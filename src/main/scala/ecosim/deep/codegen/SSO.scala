package ecosim.deep.codegen

import ecosim.deep.algo.AlgoInfo.{CodeNodeMtd, CodeNodePos, EdgeInfo}
import ecosim.deep.algo.{Algo, AlgoInfo, CallMethod, Send}
import ecosim.deep.member.{Actor, ActorType, LiftedMethod}
import ecosim.deep.IR.Predef._
import javassist.CtNewMethod
import squid.lib.MutVar

import scala.collection.mutable.{ArrayBuffer, ListBuffer}


//plan for tomorrow:
// 1. organize code better: everything goes into a function which is an obvious step
// 2. make sure not to copy the same methods multiple times - that could fix the problem with blocking sends
// 3. find out why the blocking send doesn't work
// 4. probably final: the to do 2 in line 19
// 5. probably final2: do it iteratively

//TODO maybe when copying a method copy it into my ActorType to stay consistent
//TODO when copying a method, check if it contains a call method. if it does, copy the called method too. watch out for recursion
class SSO extends StateMachineElement() {

  override def run(compiledActorGraphs: List[CompiledActorGraph]): List[CompiledActorGraph] = {
    val graphs = compiledActorGraphs.map(element => {
      optimizeElement(element, compiledActorGraphs.filter(otherElement => otherElement != element))
    })
    graphs.foreach(g => GraphDrawing.drawGraph(g.graph, g.name+" SSOmerged"))
    graphs
  }

  def optimizeElement(element: CompiledActorGraph, rest: List[CompiledActorGraph]): CompiledActorGraph = {
    def rewriteCallMethod(edges: ArrayBuffer[EdgeInfo]): ArrayBuffer[EdgeInfo] = {
      edges.foreach(edge => {
        edge.code = edge.code.rewrite({
          case code"ecosim.deep.algo.Instructions.setMethodParam(${Const(a)}, ${Const(b)}, $c)" => {
            val variable: Variable[MutVar[Any]] = CreateActorGraphs.methodVariableTable(a)(b)
            code"$variable := $c"
          }
          case code"ecosim.deep.algo.Instructions.saveMethodParam(${Const(a)}, ${Const(b)}, $c)" => {
            val stack: ArrayBuffer[Variable[ListBuffer[Any]]] = CreateActorGraphs.methodVariableTableStack(a)
            val varstack: Variable[ListBuffer[Any]] = stack(b)
            code"$varstack.prepend($c);"
          }
          case code"ecosim.deep.algo.Instructions.restoreMethodParams(${Const(a)})" => {
            val stack: ArrayBuffer[Variable[ListBuffer[Any]]] = CreateActorGraphs.methodVariableTableStack(a)
            val initCode: OpenCode[Unit] = code"()"
            stack.zipWithIndex.foldRight(initCode)((c, b) => {
              val variable: Variable[MutVar[Any]] = CreateActorGraphs.methodVariableTable(a)(c._2)
              val ab = c._1
              code"$ab.remove(0); if(!$ab.isEmpty) {$variable := $ab(0)}; $b; ()"
            })
          }
        })
      })
      edges
    }

    def mtdToPosNodes(graph: ArrayBuffer[EdgeInfo]) = {
      graph.foreach(edge => {
        edge.from match {
          case c: CodeNodeMtd =>
            val id = c.id
            val methodGraph = graph.filter(edge1 => edge1.methodId1 == id)
            if (!c.end) {
              edge.from = CodeNodePos(methodGraph.head.from.asInstanceOf[CodeNodePos].pos)
            }
            //case of interest: jump from the end of the method
            else {
              edge.from = CodeNodePos(methodGraph.last.to.asInstanceOf[CodeNodePos].pos)
            }
          case _ =>
        }
        edge.to match {
          case c: CodeNodeMtd =>
            val id = c.id
            val methodGraph = graph.filter(edge1 => edge1.methodId1 == id)
            //case of interest: jump to the beginning of the method
            if (!c.end) {
              edge.to = CodeNodePos(methodGraph.head.from.asInstanceOf[CodeNodePos].pos)
            }
            else {
              edge.to = CodeNodePos(methodGraph.last.to.asInstanceOf[CodeNodePos].pos)
            }
          case _ =>
        }
      })
    }

    def copyMethod(element: CompiledActorGraph, neededElement: CompiledActorGraph, methodId: Int): Int = {
      val newMethodId = simulation.Generator.getNextMethodId
      element.variables = (element.variables ::: neededElement.variables).distinct
      element.variables2 = (element.variables2 ::: neededElement.variables2).distinct
      element.actorTypes = (element.actorTypes ::: neededElement.actorTypes).distinct
      val methodToCopyGraph = neededElement.graph.filter(edge2 => edge2.methodId1 == methodId).map(edge2 => edge2.myCopy())
      //TODO might need to fix this. ask markus if this will always be okay
      val oldPos = methodToCopyGraph.head.from.asInstanceOf[CodeNodePos].pos
      var newFreePos = element.freePosition
      //moving the tos and froms to fit this graph
      methodToCopyGraph.foreach(edge1 => {
        edge1.methodId1 = newMethodId
        edge1.from match {
          case c: CodeNodePos =>
            edge1.from = CodeNodePos(c.pos - oldPos + element.freePosition)
          case _ =>
        }
        edge1.to match {
          case c: CodeNodePos =>
            edge1.to = CodeNodePos(c.pos - oldPos + element.freePosition)
            newFreePos = edge1.to.asInstanceOf[CodeNodePos].pos + 1
          case _ =>
        }
      })
      element.freePosition = newFreePos
      //add the new method to the graph
      element.graph = element.graph ++ methodToCopyGraph
      newMethodId
    }

    def createCallMethodNodes(newMethodId: Int, oldMethodId: Int, sendEdge: EdgeInfo, send: Send[_]): ArrayBuffer[EdgeInfo] = {

      AlgoInfo.resetData()
      CallMethod[Any](newMethodId, send.argss).codegen
      val newEdges = AlgoInfo.stateGraph.map(edge1 => {
        //move them to the right place
        edge1.from match {
          //TODO check if the cast can be safely done
          case c: CodeNodePos => edge1.from = CodeNodePos(c.pos + sendEdge.from.asInstanceOf[CodeNodePos].pos)
          case _ =>
        }
        edge1.to match {
          //TODO check if the cast can be safely done
          case c: CodeNodePos => edge1.to = CodeNodePos(c.pos + sendEdge.from.asInstanceOf[CodeNodePos].pos)
          case _ =>
        }
        edge1
      })
      AlgoInfo.resetData()
//      newEdges.head.storePosRef = List(List(newEdges.tail.head))
      CreateActorGraphs.methodVariableTableStack(newMethodId) = CreateActorGraphs.methodVariableTableStack(oldMethodId)
      CreateActorGraphs.methodVariableTable(newMethodId) = CreateActorGraphs.methodVariableTable(oldMethodId)
      rewriteCallMethod(newEdges)
    }

    def moveGraphPositions(moveAmmount: Int, moveThreshold: Int): Unit = {
      element.freePosition += moveAmmount
      element.graph.foreach(edge2 => {
        edge2.from match {
          case c: CodeNodePos =>
            if (c.pos > moveThreshold)
              edge2.from = CodeNodePos(c.pos + moveAmmount)
          case _ =>
        }
        edge2.to match {
          case c: CodeNodePos =>
            if (c.pos > moveThreshold)
              edge2.to = CodeNodePos(c.pos + moveAmmount)
          case _ =>
        }
      })
    }

    //map of all the changes made
    var changes: Map[EdgeInfo, List[EdgeInfo]] = Map()
    //for each copied method, saves the originalId -> newId, as to not copy the same method twice
    var oldToNewMtdIds: Map[Int,Int] = Map()

    element.graph.foreach(edge => {
      if (edge.sendInfo != null) {
        //TODO check if the other element is stateless
        //TODO copy method and variables
        //steps:
        //1. find the actorType which has this method
        //2. in his graph find the part of it that is this method
        //3. append that part to this graph
        //3.1 translate all froms and tos so they fit this graph
        //4. do variables....................
        //5. prepare the callmethod part of the graph
        //5.1 set the from and to to this send
        //5.2 rewrite the compile time code to runtime code ((((OOOR just copy it from the stateless server - a tad easier, a tad more restrictive)))) - try normal first
        val send = edge.sendInfo._1
        val methodId = send.methodId
        val neededElement = rest.filter(element => element.actorTypes.exists(actorType => actorType.methods.exists(method => method.methodId == methodId))).headOption
        if (neededElement.isEmpty) throw new Exception("Theres a message requesting a non existent method")
        var newMethodId = -1
        if (oldToNewMtdIds.get(methodId).isDefined) {
          newMethodId = oldToNewMtdIds(methodId)
        }
        else {
          newMethodId = copyMethod(element, neededElement.get, methodId)
          oldToNewMtdIds = oldToNewMtdIds + (methodId -> newMethodId)
        }

        //special case - this send has already been replaced, these edges are left overs that need to be removed
        if (send.blocking && !edge.sendInfo._2) {
          changes = changes + (edge -> List())
        }
        else {
          val newEdges = createCallMethodNodes(newMethodId, methodId, edge, send)
          if (send.blocking) {
            if (edge.sendInfo._2) {
              //TODO check if the cast can be safely done
              val moveThreshold = newEdges.head.from.asInstanceOf[CodeNodePos].pos
              val moveAmmount = -2
              moveGraphPositions(moveAmmount, moveThreshold)
              changes = changes + (edge -> newEdges.toList)
            }
          } else {
            //TODO check if the cast can be safely done
            val moveThreshold = newEdges.head.from.asInstanceOf[CodeNodePos].pos
            val moveAmmount = 1
            moveGraphPositions(moveAmmount, moveThreshold)
            changes = changes + (edge -> newEdges.toList)
          }
        }
      }
    })

    changes.foreach(change => {
      element.graph = element.graph.flatMap(edge => if(edge == change._1) change._2 else List(edge))
    })
    mtdToPosNodes(element.graph)
    element
  }



}
