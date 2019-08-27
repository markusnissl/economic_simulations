package ecosim.deep.algo

import ecosim.deep.IR.Predef._

case class NoOp[A:CodeType]() extends Algo[A] {

  /**
    * Just do nothing, it will not return anything in comaprison to scala code, which will return an empty unit
    */
  override def codegen: Unit = {}
}
