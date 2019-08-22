package ecosim.deep.member

import ecosim.deep.IR
import ecosim.deep.IR.Predef._

case class State[A](sym: IR.MtdSymbol, init: OpenCode[A])(implicit val tpe: CodeType[A]) {
  override def toString = s"var ${sym.asMethodSymbol.owner.name}.${sym.asMethodSymbol.name}"
}
