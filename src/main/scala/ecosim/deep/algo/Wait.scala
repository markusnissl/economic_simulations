package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._

/**
  * Increases timer by cde time
  * @param cde amount of time to wait
  */
case class Wait(cde: OpenCode[Int]) extends Algo[Unit] {

  override def codegen: List[IR.Predef.OpenCode[Unit]] = {
    val met: OpenCode[Unit] = code"${AlgoInfo.timeVar} := (${AlgoInfo.timeVar}!) + $cde; ()"
    AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("wait met", AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(AlgoInfo.posCounter+1), met, waitEdge = true))
    AlgoInfo.nextPos
    List(met)
  }
}
