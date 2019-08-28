package ecosim.deep.codegen

import ecosim.deep.IR.Predef._
import ecosim.deep.algo.AlgoInfo.{CodeNodePos, EdgeInfo}

import scala.collection.mutable.ArrayBuffer

object MergeActors {

  /**
    * This functions generates an abstract graph with only wait edges
    * This is required to generate a merged state machine, where only the wait edges generate a new state
    */
  def waitGraph(graph: ArrayBuffer[EdgeInfo]): ArrayBuffer[EdgeInfo] = {
    val graphStart = graph.groupBy(_.from.asInstanceOf[CodeNodePos].pos)
    val newGraph:ArrayBuffer[EdgeInfo] = ArrayBuffer[EdgeInfo]()
    var startNodes:List[Int] = List()

    def waitGraphInner(currentNode: Int, edgeInfo: EdgeInfo, visited: List[Int]): Unit = {
      //We are looping
      if (visited.contains(currentNode)) {
        return
      }
      if (edgeInfo == null) {
        //Already handled that node as start node
        if (startNodes.contains(currentNode)) {
          return
        }
        startNodes = currentNode :: startNodes
      }

      val startNode = graphStart(currentNode)
      startNode.foreach(edge => {
        var newEdgeInfo = edge
        if (edgeInfo != null) {
          var newCond = edgeInfo.cond
          if (newCond == null && edge.cond != null) {
            newCond = edge.cond
          } else if (newCond != null && edge.cond != null) {
            newCond = code"${edgeInfo.cond} && ${edge.cond}"
          }
          newEdgeInfo = EdgeInfo(edgeInfo.label + ", " + edge.label, edgeInfo.from, edge.to, code"${edgeInfo.code}; ${edge.code}", edge.waitEdge, false, newCond, edgeInfo.storePosRef ::: edge.storePosRef)
        }
        if (edge.waitEdge) {
          newGraph.append(newEdgeInfo)
          waitGraphInner(edge.to.asInstanceOf[CodeNodePos].pos, null, List())
        } else {
          waitGraphInner(edge.to.asInstanceOf[CodeNodePos].pos, newEdgeInfo, currentNode :: visited)
        }
      })
    }

    waitGraphInner(0, null, List())

    newGraph
  }

  def generateMergedStateMachine(graph1:ArrayBuffer[EdgeInfo], graph2: ArrayBuffer[EdgeInfo]): ArrayBuffer[EdgeInfo] = {
    val startGraph1 = graph1.groupBy(_.from.asInstanceOf[CodeNodePos].pos)
    val startGraph2 = graph2.groupBy(_.from.asInstanceOf[CodeNodePos].pos)

    val mergedGraph:ArrayBuffer[EdgeInfo] = ArrayBuffer[EdgeInfo]()

    var newStateCounter:Int = 0
    var stateMapping: Map[(Int, Int), Int] = Map[(Int, Int), Int]()

    for (stateA <- startGraph1.keys.toList.sorted) {
      for (stateB <- startGraph2.keys.toList.sorted) {
        for (symbol1 <- startGraph1(stateA)) {
          for (symbol2 <- startGraph2(stateB)) {

            val startPos = stateMapping.getOrElse((stateA, stateB), newStateCounter)
            if (startPos == newStateCounter) {
              newStateCounter = newStateCounter+1
              stateMapping = stateMapping + ((stateA, stateB) -> startPos)
            }
            val endStateA = symbol1.to.asInstanceOf[CodeNodePos].pos
            val endStateB = symbol2.to.asInstanceOf[CodeNodePos].pos

            val endPos = stateMapping.getOrElse((endStateA, endStateB), newStateCounter)
            if (endPos == newStateCounter) {
              newStateCounter = newStateCounter+1
              stateMapping = stateMapping + ((endStateA, endStateB) -> endPos)
            }

            var newCond = symbol1.cond
            if (newCond == null && symbol2.cond != null) {
              newCond = symbol2.cond
            } else if (newCond != null && symbol2.cond != null) {
              newCond = code"${symbol1.cond} && ${symbol2.cond}"
            }

            val newEdgeInfo = EdgeInfo(symbol1.label + "--" + symbol2.label, CodeNodePos(startPos), CodeNodePos(endPos), code"${symbol1.code}; ${symbol2.code}",true, false, newCond, symbol1.storePosRef ::: symbol2.storePosRef)
            mergedGraph.append(newEdgeInfo)
          }
        }
      }
    }

    //This removes unreached states
    waitGraph(mergedGraph)
  }
}
