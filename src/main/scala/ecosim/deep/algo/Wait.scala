package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._

case class Wait(cde: OpenCode[Int]) extends Algo[Unit] {

  override def codegen: List[IR.Predef.OpenCode[Unit]] = {
    val met: OpenCode[Unit] = code"${AlgoInfo.timeVar} := (${AlgoInfo.timeVar}!) + $cde; ()"
    AlgoInfo.merger.append((true, false))

    List(met)
  }
}
