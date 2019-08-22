package ecosim.deep.member

import ecosim.deep.IR
import ecosim.deep.IR.Predef._
import ecosim.deep.algo.Algo

abstract class LiftedMethod[R](val cls: IR.TopLevel.Clasz[_], val body: Algo[R], val blocking: Boolean, val methodId:Int)(implicit val R:CodeType[R]) {
  def sym: IR.MtdSymbol = mtd.symbol

  val mtd: cls.Method[R, cls.Scp]


  override def hashCode() = sym.hashCode()

  override def equals(that: Any) = that match {
    case that: LiftedMethod[_] => that.sym === sym
  }

  override def toString = s"${sym.asMethodSymbol.owner.name}.${sym.asMethodSymbol.name}"

}
