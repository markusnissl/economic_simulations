package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._
import squid.lib.MutVar

case class Forever(body: Algo[_]) extends Algo[Unit] {

  /**
    * 1. Save current position
    * 2. Execute body
    * 3. Jump to position saved in 1
    * @return a list of opencode, containing individual program steps
    */
  override def codegen: List[IR.Predef.OpenCode[Unit]] = {

    val f0 = AlgoInfo.pushCurrent
    AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("Forever f0", AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(AlgoInfo.posCounter+1), f0))
    val tmpPos = AlgoInfo.posCounter
    AlgoInfo.nextPos
    val x = body.codegen

    val f2 = AlgoInfo.restorePosition
    AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("Forever f2", AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(tmpPos), f2))
    AlgoInfo.nextPos

    List(f0) ::: x ::: List(f2)
  }

}
