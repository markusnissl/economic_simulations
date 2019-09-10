package ecosim.deep.codegen

import ecosim.deep.algo.AlgoInfo.{CodeNode, CodeNodeMtd, CodeNodePos, EdgeInfo}
import ecosim.deep.algo.{Algo, AlgoInfo, CallMethod, Send}
import ecosim.deep.IR.Predef._
import ecosim.deep.codegen.CreateActorGraphs.{MutVarType, methodVariableTable, methodVariableTableStack}
import ecosim.deep.member.Actor

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

//TODO when copying a method copy it into my ActorType to stay consistent
//FIXME this part relies on methodVariableTable and methodVariableTableStack which are global, and might not be valid at this point
class SSO extends StateMachineElement() {

  //TODO change the structure to something more clear. e.g. its hard to see here what the usage of these structurs will be
  //map of all the changes made
  var changes: Map[EdgeInfo, List[EdgeInfo]] = Map()
  //for each copied method, saves the originalId -> newId, as to not copy the same method twice
  var oldToNewMtdIds: Map[String, Map[Int,Int]] = Map()
  var optimizationDone = false

  override def run(compiledActorGraphs: List[CompiledActorGraph]): List[CompiledActorGraph] = {
    var graphs: List[CompiledActorGraph] = List()
    while (!optimizationDone) {
      optimizationDone = true
      graphs = compiledActorGraphs.map(element => {
        optimizeElement(element, compiledActorGraphs.filter(otherElement => otherElement != element))
      })
    }
    graphs.foreach(g => g.graph.foreach(edge => edge.positionStack = g.positionStack.head))
    graphs.foreach(g => mtdToPosNodes(g.graph))
//    graphs.foreach(g => GraphDrawing.drawGraph(g.graph, g.name+" SSOmerged"))
    graphs
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

  def surroundWithWaitEdges(edges: ArrayBuffer[EdgeInfo], methodId: Int): ArrayBuffer[EdgeInfo] = {
    val firstFrom = edges.head.from
    edges.foreach(edge1 => {
      edge1.to match {
        case c: CodeNodePos =>
          edge1.to = CodeNodePos(c.pos + 1)
        case _ =>
      }
      edge1.from match {
        case c: CodeNodePos =>
          edge1.from = CodeNodePos(c.pos + 1)
        case _ =>
      }
    })
    val w1 = AlgoInfo.EdgeInfo("wait", firstFrom, edges.head.from, code"()", waitEdge = true, methodId1 = methodId)
    val w2 = AlgoInfo.EdgeInfo("wait", edges.last.to, CodeNodePos(edges.last.to.asInstanceOf[CodeNodePos].pos + 1), code"()", waitEdge = true, methodId1 = methodId)
    edges.prepend(w1)
    edges.append(w2)
    edges
  }

  def optimizeElement(element: CompiledActorGraph, rest: List[CompiledActorGraph]): CompiledActorGraph = {
    def rewriteCallMethod(edges: ArrayBuffer[EdgeInfo]): ArrayBuffer[EdgeInfo] = {
      edges.foreach(edge => {
        edge.code = edge.code.rewrite({
          case code"ecosim.deep.algo.Instructions.setMethodParam(${Const(a)}, ${Const(b)}, $c)" => {
            val variable: MutVarType[_] = methodVariableTable(a)(b)

            variable match {
              case v:MutVarType[a] => {
                code"${v.variable} := $c.asInstanceOf[${v.codeType}]"
              }
              case _ => throw new RuntimeException("Illegal state")
            }
          }
          case code"ecosim.deep.algo.Instructions.saveMethodParam(${Const(a)}, ${Const(b)}, $c)" => {
            val stack: ArrayBuffer[Variable[ListBuffer[Any]]] = methodVariableTableStack(a)
            val varstack: Variable[ListBuffer[Any]] = stack(b)
            code"$varstack.prepend($c);"
          }
          case code"ecosim.deep.algo.Instructions.restoreMethodParams(${Const(a)})" => {
            val stack: ArrayBuffer[Variable[ListBuffer[Any]]] = methodVariableTableStack(a)
            val initCode: OpenCode[Unit] = code"()"
            stack.zipWithIndex.foldRight(initCode)((c, b) => {
              val variable: MutVarType[_] = methodVariableTable(a)(c._2)
              val ab = c._1
              variable match {
                case v:MutVarType[a] => {
                  code"$ab.remove(0); if(!$ab.isEmpty) {${v.variable} := $ab(0).asInstanceOf[${v.codeType}]}; $b; ()"
                }
                case _ => throw new RuntimeException("Illegal state")
              }
            })
          }
        })

      })
      edges
    }

    def copyMethod(element: CompiledActorGraph, neededElement: CompiledActorGraph, methodId: Int): Int = {
      //first check if its already copied. if it is, just return the local method id
      if (oldToNewMtdIds.getOrElse(element.name, Map[Int, Int]()).get(methodId).isDefined) {
        oldToNewMtdIds(element.name)(methodId)
      }
      else {
        val newMethodId = simulation.Generator.getNextMethodId
        element.variables = (element.variables ::: neededElement.variables).distinct
        element.variables2 = (element.variables2 ::: neededElement.variables2).distinct
        neededElement.actorTypes.foreach(at => {
          element.actorTypes.head.states = (element.actorTypes.head.states ::: at.states).distinct
        })
        val methodToCopyGraph = neededElement.graph.filter(edge2 => edge2.methodId1 == methodId).map(edge2 => edge2.myCopy())
        val oldPos = methodToCopyGraph.head.from.asInstanceOf[CodeNodePos].pos
        var newFreePos = element.freePosition
        //moving the tos and froms to fit this graph,
        // fixing the method id,
        // fixing the reference from another actor to self,
        // from another actors response message to this response message
        // and from another actors return value this return value
        methodToCopyGraph.foreach(edge1 => {
          edge1.methodId1 = newMethodId
          neededElement.actorTypes.foreach( at => {
            val selfThis = element.actorTypes.head.self.asInstanceOf[Variable[Actor]]
            val otherThis = at.self.asInstanceOf[Variable[Actor]]
            //FIXME this will maybe break the app after merging a few graphs together
            // because here theres no check if the right return value is taken (from all of the merged graphs)
            val selfReturnValue = element.returnValue.head
            val otherReturnValue = neededElement.returnValue.head
            val selfResponseMessage = element.responseMessage.head
            val otherResponseMessage = neededElement.responseMessage.head
            if (edge1.code != null) {
              edge1.code = edge1.code.subs(otherThis).~>(selfThis.toCode)
              edge1.code = edge1.code.subs(otherReturnValue).~>(selfReturnValue.toCode)
              edge1.code = edge1.code.subs(otherResponseMessage).~>(selfResponseMessage.toCode)
            }
            if (edge1.cond != null) {
              edge1.cond = edge1.cond.subs(otherThis).~>(selfThis.toCode)
              edge1.cond = edge1.cond.subs(otherReturnValue).~>(selfReturnValue.toCode)
              edge1.cond = edge1.cond.subs(otherResponseMessage).~>(selfResponseMessage.toCode)
            }
          })
          edge1.from match {
            case c: CodeNodePos =>
              edge1.from = CodeNodePos(c.pos - oldPos + element.freePosition)
              newFreePos = edge1.from.asInstanceOf[CodeNodePos].pos + 1
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
        //remember that the method was added. do it here, so in case of recursion it will not get copied again, just called
        oldToNewMtdIds = oldToNewMtdIds + (element.name -> (oldToNewMtdIds.getOrElse(element.name, Map[Int,Int]()) + (methodId -> newMethodId)))

        //after copying the graph check if it has a call method somewhere,
        // if it does, that method has to be copied from the other actor as well
        // and the edges calling the method need to be changed
        var methodCallIndex = 0
        methodToCopyGraph.foreach(edge1 => {
          if (edge1.methodCallInfo._1 != null) {
            val calledMethodId = edge1.methodCallInfo._1.methodId
            var copiedMethodId = -1
            //if it was already copied, no need to do it again
            if (oldToNewMtdIds.getOrElse(element.name, Map[Int,Int]()).get(calledMethodId).isDefined) {
              copiedMethodId = oldToNewMtdIds(element.name)(calledMethodId)
            } else {
              copiedMethodId = copyMethod(element, neededElement, calledMethodId)
              oldToNewMtdIds = oldToNewMtdIds + (element.name -> (oldToNewMtdIds.getOrElse(element.name, Map[Int,Int]()) + (methodId -> newMethodId)))
            }
            edge1.label = edge1.label.replaceFirst(calledMethodId.toString, copiedMethodId.toString)
            edge1.methodCallInfo = (CallMethod[Any](copiedMethodId, edge1.methodCallInfo._1.argss), methodCallIndex)
            methodCallIndex = (methodCallIndex + 1) % 3
            //if this edge contains a CodeNodeMtd, its method id has to be changed
            edge1.from match {
              case c: CodeNodeMtd =>
                edge1.from = c.copy(id = copiedMethodId)
              case _ =>
            }
            edge1.to match {
              case c: CodeNodeMtd =>
                edge1.to = c.copy(id = copiedMethodId)
              case _ =>
            }
          }
        })
        //need to fix the restore positions of each method call
        // and also for each copied method call, set the positions correctly
        val copiedMethodCalls = methodToCopyGraph.groupBy(edge1 => edge1.methodCallInfo._1)
        copiedMethodCalls.foreach(group => {
          if (group._1 != null) {
            setPosRef(group._2)
            setMtdRef(group._2)
          }
        })
        newMethodId
      }
    }

    /*** given a list of call method edges, it will set them the right storePosRef
      *  called after copying a callMethod from another graph
      * @param callMethodEdges - list of call method edges, where each call method will have exactly 3 edges
      *                        (otherwise it will not work). the second of the three edges has to be the pos ref
      */
    def setPosRef(callMethodEdges: ArrayBuffer[EdgeInfo]): Unit = {
      val (currentMethodEdges, restMethodEdges) = callMethodEdges.splitAt(3)
      currentMethodEdges.head.storePosRef = List(List(currentMethodEdges.tail.head))
      if (restMethodEdges.nonEmpty) {
        setPosRef(restMethodEdges)
      }
    }

    def setMtdRef(callMethodEdges: ArrayBuffer[AlgoInfo.EdgeInfo]): Unit = {
      val (currentMethodEdges, restMethodEdges) = callMethodEdges.splitAt(3)
      val methodId = currentMethodEdges.head.methodCallInfo._1.methodId
      currentMethodEdges.head.to = CodeNodeMtd(methodId, false)
      currentMethodEdges.tail.head.from = CodeNodeMtd(methodId, true)
      if (restMethodEdges.nonEmpty) {
        setMtdRef(restMethodEdges)
      }
    }


    def createCallMethodNodes(newMethodId: Int, oldMethodId: Int, sendEdge: EdgeInfo, send: Send[_]): ArrayBuffer[EdgeInfo] = {

      AlgoInfo.resetData()
      CallMethod[Any](newMethodId, send.argss).codegen
      val newEdges = AlgoInfo.stateGraph.map(edge1 => {
        //since its replacing the sendEdge, these edges have to belong to the same method as that sendEdge
        edge1.methodId1 = sendEdge.methodId1
        //move them to the right place
        edge1.from match {
          case c: CodeNodePos => edge1.from = CodeNodePos(c.pos + sendEdge.from.asInstanceOf[CodeNodePos].pos)
          case _ =>
        }
        edge1.to match {
          case c: CodeNodePos => edge1.to = CodeNodePos(c.pos + sendEdge.from.asInstanceOf[CodeNodePos].pos)
          case _ =>
        }
        edge1
      })
      AlgoInfo.resetData()
      //rewrite the compile only methods to runtime methods
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


    element.graph.foreach(edge => {
      if (edge.sendInfo != null) {
        //steps:
        //1. find the actorType which has this method
        //2. in his graph find the part of it that is this method
        //3. append that part to this graph
        //3.1 translate all froms and tos so they fit this graph
        //4. do variables
        //5. prepare the callmethod part of the graph
        //5.1 set the from and to to this send
        val send = edge.sendInfo._1
        val methodId = send.methodId
        val neededElement = (element :: rest).find(element => element.graph.exists(edge2 => edge2.methodId1 == methodId))
        if (neededElement.isEmpty) throw new Exception("Theres a message requesting a non existent method")
        if (neededElement.get.actorTypes.forall(at => at.stateless)) {
          optimizationDone = false //there will be some copying or changing done, so this might not be the last iteration
          val newMethodId = copyMethod(element, neededElement.get, methodId)
          //special case - this send has already been replaced, these edges are left overs that need to be removed
          if (send.blocking && !edge.sendInfo._2) {
            changes = changes + (edge -> List())
          }
          else {
            var newEdges = createCallMethodNodes(newMethodId, methodId, edge, send)
            if (send.blocking) {
              if (edge.sendInfo._2) {
                //add 2 wait edges
                surroundWithWaitEdges(newEdges, edge.methodId1)
                val moveThreshold = newEdges.head.from.asInstanceOf[CodeNodePos].pos
                val moveAmmount = 0
                moveGraphPositions(moveAmmount, moveThreshold)
                changes = changes + (edge -> newEdges.toList)
              }
            } else {
              val moveThreshold = newEdges.head.from.asInstanceOf[CodeNodePos].pos
              val moveAmmount = 1
              moveGraphPositions(moveAmmount, moveThreshold)
              changes = changes + (edge -> newEdges.toList)
            }
          }
        }
      }
    })
    changes.foreach(change => {
      element.graph = element.graph.flatMap(edge => if(edge == change._1) change._2 else List(edge))
    })
    element
  }



}
