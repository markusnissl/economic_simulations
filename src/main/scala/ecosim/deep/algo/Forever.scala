package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._
import squid.lib.MutVar

case class Forever(body: Algo[_]) extends Algo[Unit] {

  /**
    * 1. Save current position
    * 2. Execute body
    * 3. Jump to position saved in 1
    */
  override def codegen: Unit = {

    val tmpPos = AlgoInfo.posCounter

    body.codegen

    AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("Forever", AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(tmpPos), code"()"))
  }

}
