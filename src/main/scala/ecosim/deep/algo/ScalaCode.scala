package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._

case class ScalaCode[A: CodeType](cde: OpenCode[A]) extends Algo[A] {

  /**
    * Runs the code provided and saves the result as return value.
    * @return a list of opencode, containing individual program steps
    */
  override def codegen: List[IR.Predef.OpenCode[Unit]] = {
    val met: OpenCode[Unit] = code"""${AlgoInfo.returnValue} := $cde; ()"""
    AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("ScalaCode met",AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(AlgoInfo.posCounter+1), met))
    AlgoInfo.nextPos
    List(met)
  }
}
