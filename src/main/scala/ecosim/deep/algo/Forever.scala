package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._
import squid.lib.MutVar

case class Forever(body: Algo[_]) extends Algo[Unit] {

  override def codegen: List[IR.Predef.OpenCode[Unit]] = {

    AlgoInfo.merger.append((false, true))
    val x = body.codegen
    AlgoInfo.merger.append((true, false))

    List(AlgoInfo.pushCurrent) ::: x ::: List(AlgoInfo.restorePosition)
  }

}
