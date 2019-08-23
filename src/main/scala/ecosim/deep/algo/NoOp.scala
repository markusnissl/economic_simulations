package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._

case class NoOp[A:CodeType]() extends Algo[A] {

  /**
    * Just do nothing, it will not return anything in comaprison to scala code, which will return an empty unit
    * @return a list of opencode, containing individual program steps
    */
  override def codegen: List[IR.Predef.OpenCode[Unit]] = List()
}
