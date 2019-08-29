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
    var edgeList = List[(Int, Int)]()

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
        //Remove all data about the node, make it as abstract as possible
        val edgeTargetPos = edge.to.asInstanceOf[CodeNodePos].pos
        var newEdgeInfo = EdgeInfo(currentNode + "->" + edgeTargetPos, edge.from, edge.to, null, edge.waitEdge, false, null, List())
        if (edgeInfo != null) {
          newEdgeInfo = EdgeInfo(edgeInfo.from.asInstanceOf[CodeNodePos].pos + "->" + edgeTargetPos, edgeInfo.from, edge.to, null, edge.waitEdge, false, null, List())
        }
        if (edge.waitEdge) {
          if (!edgeList.contains((newEdgeInfo.from.asInstanceOf[CodeNodePos].pos, newEdgeInfo.to.asInstanceOf[CodeNodePos].pos))) {
            newGraph.append(newEdgeInfo)
            edgeList = (newEdgeInfo.from.asInstanceOf[CodeNodePos].pos, newEdgeInfo.to.asInstanceOf[CodeNodePos].pos) :: edgeList
          }
          waitGraphInner(edgeTargetPos, null, List())
        } else {
          waitGraphInner(edgeTargetPos, newEdgeInfo, currentNode :: visited)
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

            val newEdgeInfo = EdgeInfo(symbol1.label + "|" + symbol2.label, CodeNodePos(startPos), CodeNodePos(endPos), null, true, false, null, symbol1.storePosRef ::: symbol2.storePosRef)
            mergedGraph.append(newEdgeInfo)
          }
        }
      }
    }

    val mergedGraphReachableList = ArrayBuffer[Int]()
    val startMergedgraph = mergedGraph.groupBy(_.from.asInstanceOf[CodeNodePos].pos)
    def calculateReachable(currentNode: Int): Unit = {
      val node = startMergedgraph(currentNode)
      if (mergedGraphReachableList.contains(currentNode)) {
        return
      }
      mergedGraphReachableList.append(currentNode)
      node.foreach(edge => calculateReachable(edge.to.asInstanceOf[CodeNodePos].pos))
    }
    calculateReachable(0)

    var mergedGraphReachable = ArrayBuffer[EdgeInfo]()
    //This removes unreached states
    mergedGraph.foreach(edge => {
      if(mergedGraphReachableList.contains(edge.from.asInstanceOf[CodeNodePos].pos)) {
        if(mergedGraphReachableList.contains(edge.to.asInstanceOf[CodeNodePos].pos)) {
          mergedGraphReachable.append(edge)
        }
      }
    })

    mergedGraphReachable
  }
}
