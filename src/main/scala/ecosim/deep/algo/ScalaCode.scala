package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._

case class ScalaCode[A: CodeType](cde: OpenCode[A]) extends Algo[A] {

  override def codegen: List[IR.Predef.OpenCode[Unit]] = {
    val met: OpenCode[Unit] = code"""${AlgoInfo.returnValue} := $cde; ()"""
    AlgoInfo.merger.append((true, true))

    List(met)
  }
}
