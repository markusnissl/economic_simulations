package ecosim.deep.codegen

import ecosim.deep.IR.Predef._
import ecosim.deep.algo.AlgoInfo.{CodeNodePos, EdgeInfo, MergeInfo}

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

  def generateMergedStateMachine(graph1:ArrayBuffer[EdgeInfo], graph2: ArrayBuffer[EdgeInfo]): ArrayBuffer[MergeInfo] = {
    val startGraph1 = graph1.groupBy(_.from.asInstanceOf[CodeNodePos].pos)
    val startGraph2 = graph2.groupBy(_.from.asInstanceOf[CodeNodePos].pos)

    val mergedGraph:ArrayBuffer[MergeInfo] = ArrayBuffer[MergeInfo]()

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

            val newEdgeInfo = MergeInfo(CodeNodePos(startPos), CodeNodePos(endPos), (symbol1.from.asInstanceOf[CodeNodePos],symbol1.to.asInstanceOf[CodeNodePos]), (symbol2.from.asInstanceOf[CodeNodePos],symbol2.to.asInstanceOf[CodeNodePos]))
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

    var mergedGraphReachable = ArrayBuffer[MergeInfo]()
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

  def combineActors(mergedGraph:ArrayBuffer[MergeInfo], graph1:ArrayBuffer[EdgeInfo], graph2:ArrayBuffer[EdgeInfo]): ArrayBuffer[EdgeInfo] = {
    val graph1Start = graph1.groupBy(_.from.asInstanceOf[CodeNodePos].pos)
    val graph2Start = graph2.groupBy(_.from.asInstanceOf[CodeNodePos].pos)
    val graph1Reachable: collection.mutable.Map[Int, List[Int]] = collection.mutable.Map[Int, List[Int]]()
    val graph2Reachable: collection.mutable.Map[Int, List[Int]] = collection.mutable.Map[Int, List[Int]]()

    def calculateReachableStates(graphStart: Map[Int, ArrayBuffer[EdgeInfo]], currentNode: Int, visited: List[Int]): List[Int] = {
      //Node already visited
      if (visited.contains(currentNode)) {
        return Nil
      }

      val edges = graphStart(currentNode)
      var reachable:List[Int] = Nil

      edges.foreach(edge => {
        if (edge.waitEdge) {
          reachable = edge.to.asInstanceOf[CodeNodePos].pos :: reachable
        } else {
          reachable = calculateReachableStates(graphStart, edge.to.asInstanceOf[CodeNodePos].pos, currentNode :: visited)
        }
      })
      reachable
    }

    graph1Start.keys.foreach(x => graph1Reachable(x) = calculateReachableStates(graph1Start, x, Nil))
    graph2Start.keys.foreach(x => graph2Reachable(x) = calculateReachableStates(graph2Start, x, Nil))

    val mergedGraphStart = mergedGraph.groupBy(_.from.pos)

    var handledGraphStarts = List[Int]()

    var reachedStatesGlobal = Map[Int, Map[Int, Int]]()
    var startGraphGlobalMap = Map[Int, Int]()
    var posCounter = 0
    val globalGraph:ArrayBuffer[EdgeInfo] = ArrayBuffer[EdgeInfo]()

    def generateGraph(fromPos: Int): Unit = {
      val start = mergedGraphStart(fromPos)
      // A position has a steady start state per graph, it just depends on the control flow which next state is reached
      val start1Pos = start(0).graph1._1.pos
      val start2Pos = start(0).graph2._1.pos

      val graph:ArrayBuffer[EdgeInfo] = ArrayBuffer[EdgeInfo]()

      val reachableStates = start.map(x => (x.graph1._2.pos, x.graph2._2.pos))

      startGraphGlobalMap = startGraphGlobalMap + (fromPos -> posCounter)
      /**
        * Algorithm idea:
        * For graph 1 follow edges until reaching a reachable state
        * Then for graph2 follow edges until reaching a reachable state
        */
      var reachedStateTmp = 0
      var reachedStates = Map[Int, Int]()
      //var posMapping = Map[Int, Int]()

      def generateEdges(graphStart: Map[Int, ArrayBuffer[EdgeInfo]], graphPos: Int, graphStart2: Map[Int, ArrayBuffer[EdgeInfo]], graphPos2: Int, posMappingI: Map[Int, Int]): Unit = {
        val start1Edges = graphStart(graphPos)

        var posMapping = posMappingI
        val nodePos = posCounter

        start1Edges.foreach(edge => {
          val edgeTarget:Int = edge.to.asInstanceOf[CodeNodePos].pos

          if(graphStart2 != null && !reachableStates.exists(x => graph1Reachable(edgeTarget).contains(x._1))) {
            println("DBEUG: State not possible")
            return
          }
          if(graphStart2 == null && !reachableStates.filter(_._1 == reachedStateTmp).exists(x => graph2Reachable(edgeTarget).contains(x._2))) {
            println("DEBUG: State not possible")
            return
          }
          /*if (graph1Reachable(edgeTarget)) {
            return
          }*/

          val nextPos = posMapping.getOrElse(edgeTarget, posCounter + 1)
          var newPos = false
          if (nextPos > posCounter) {
            posCounter = posCounter+1
            posMapping = posMapping + (edgeTarget -> posCounter)
            newPos = true
          }

          //Since we combine two graphs, the first one is not allowed to wait
          var waitEdge = edge.waitEdge
          if (graphStart2 != null) {
            waitEdge = false
          }

          //TODO: rewrite storeposref
          graph.append(EdgeInfo(edge.label, CodeNodePos(nodePos), CodeNodePos(nextPos), edge.code, waitEdge, edge.isMethod, edge.cond, edge.storePosRef))

          if (!edge.waitEdge) {
            if(newPos) {
              generateEdges(graphStart, edgeTarget, graphStart2, graphPos2, posMapping)
            }
          } else {
            if (graphStart2 != null) {
              reachedStateTmp = edgeTarget
              generateEdges(graphStart2, graphPos2, null, 0, Map())
            } else {
              reachedStates = reachedStates + (posCounter -> start.find(z => reachedStateTmp == z.graph1._2.pos && edgeTarget == z.graph2._2.pos).get.to.pos)
            }
          }
        })
      }

      generateEdges(graph1Start, start1Pos, graph2Start, start2Pos, Map())
      reachedStatesGlobal = reachedStatesGlobal + (fromPos -> reachedStates)

      handledGraphStarts = fromPos :: handledGraphStarts

      globalGraph.appendAll(graph)

      start.foreach(edge => {
        if(!handledGraphStarts.contains(edge.to.pos)) {
          generateGraph(edge.to.pos)
        }

      })

    }

    generateGraph(0)

    reachedStatesGlobal.foreach(x => {
      x._2.foreach(edgeData => {
        globalGraph.append(EdgeInfo("", CodeNodePos(edgeData._1), CodeNodePos(startGraphGlobalMap(edgeData._2)), code"()", false, false, null, Nil))
      })
    })

    globalGraph
  }
}
