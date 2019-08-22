package ecosim.deep.algo

import ecosim.deep.IR.Predef._

abstract class Algo[A](implicit val tpe: CodeType[A]) {
  def codegen: List[OpenCode[Unit]]
}
