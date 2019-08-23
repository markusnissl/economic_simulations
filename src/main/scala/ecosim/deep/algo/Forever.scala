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

    AlgoInfo.merger.append((false, true))
    val x = body.codegen
    AlgoInfo.merger.append((true, false))

    List(AlgoInfo.pushCurrent) ::: x ::: List(AlgoInfo.restorePosition)
  }

}
