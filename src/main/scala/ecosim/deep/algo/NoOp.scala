package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._

case class NoOp[A:CodeType]() extends Algo[A] {

  override def codegen: List[IR.Predef.OpenCode[Unit]] = List()
}
